
(* Model(/Proof) check

*)


(* State *)
(* ************************************************************************ *)

type model = Dolmen_model.Env.t

type answer =
  | Unsat
  | Sat of model

type 'st answers =
  | Init
  | Response_loaded of ('st -> 'st * answer option)

type 'st t = {
  answers : 'st answers;
  hyps : (Dolmen.Std.Loc.t * Dolmen.Std.Expr.Term.t) list;
  goals : (Dolmen.Std.Loc.t * Dolmen.Std.Expr.Term.t) list;
  clauses : (Dolmen.Std.Loc.t * Dolmen.Std.Expr.Term.t list) list;
}

let empty () = {
  answers = Init;
  hyps = []; goals = []; clauses = [];
}


(* Warnings and errors *)
(* ************************************************************************ *)

let response_lang_changed =
  Report.Error.mk ~code:Code.generic ~mnemonic:"response-lang-changed"
    ~message:(fun fmt (old_lang, new_lang) ->
        Format.fprintf fmt
          "Response language changed from %s to %s (probably because of an include statement)"
          (Response.string_of_language old_lang)
          (Response.string_of_language new_lang))
    ~name:"Response language change" ()


(* Pipe *)
(* ************************************************************************ *)

module Pipe
    (State : State_intf.Check_pipe with type 'st check_state := 'st t)
    (Typing : Typer.Pipe_arg
     with type state := State.t
      and type ty := Dolmen.Std.Expr.ty
      and type ty_var := Dolmen.Std.Expr.ty_var
      and type ty_cst := Dolmen.Std.Expr.ty_cst
      and type term := Dolmen.Std.Expr.term
      and type term_var := Dolmen.Std.Expr.term_var
      and type term_cst := Dolmen.Std.Expr.term_cst
      and type formula := Dolmen.Std.Expr.formula)
    (Types : Typer.Pipe_res
     with type state := State.t
      and type ty := Dolmen.Std.Expr.ty
      and type ty_var := Dolmen.Std.Expr.ty_var
      and type ty_cst := Dolmen.Std.Expr.ty_cst
      and type term := Dolmen.Std.Expr.term
      and type term_var := Dolmen.Std.Expr.term_var
      and type term_cst := Dolmen.Std.Expr.term_cst
      and type formula := Dolmen.Std.Expr.formula)
= struct


  (* Parsing response files *)
  (* ************************************************************************ *)

  (* Code copied from loop/parser.ml.
     TODO: factorise the code. *)

  let gen_of_llist l =
    let l = ref l in
    (fun () -> match Lazy.force !l with
       | [] -> None
       | x :: r ->
         l := (lazy r); Some x
    )

  let set_lang ?loc st l =
    match State.response_lang st with
    | None -> State.set_response_lang st l
    | Some l' ->
      if l = l'
      then State.set_response_lang st l
      else State.error ?loc st response_lang_changed (l', l)

  let gen_finally (gen : 'a Gen.t) cl : 'a Gen.t =
    (* Register a finaliser for the original generator in case an exception is
       raised at some point in one of the pipes, which would prevent gen from
       reaching its end and thus prevent closing of the underlying file. *)
    let () = Gc.finalise_last cl gen in
    (* Return a new generator which wraps gen and calls the closing function
       once gen is finished. *)
    let aux () =
      match gen () with
      | Some _ as res -> res
      | None -> cl (); None
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        cl ();
        Printexc.raise_with_backtrace exn bt
    in
    aux

  let wrap_parser g = fun st ->
    match g () with
    | ret -> st, ret
    | exception Dolmen.Std.Loc.Uncaught (loc, exn, bt) ->
      let file = State.response_file_loc st in
      let st =
        State.error st ~loc:{ file; loc; } Report.Error.uncaught_exn (exn, bt)
      in
      st, None
    | exception Dolmen.Std.Loc.Lexing_error (loc, lex) ->
      let file = State.response_file_loc st in
      let st = State.error st ~loc:{ file; loc; } Parser.lexing_error lex in
      st, None
    | exception Dolmen.Std.Loc.Syntax_error (loc, perr) ->
      let file = State.response_file_loc st in
      let st = State.error st ~loc:{ file; loc; } Parser.parsing_error perr in
      st, None

  let parse prelude st =
    (* Parse the input *)
    let st', g =
      try
        match State.response_source st with
        | `Stdin ->
          let lang, file_loc, gen, _ = Response.parse_input
              ?language:(State.response_lang st)
              (`Stdin (Response.Smtlib2 `Latest))
          in
          let st = State.set_response_file_loc st file_loc in
          let st = set_lang st lang in
          st, gen
        | `Raw (filename, contents) ->
          let lang =
            match State.response_lang st with
            | Some l -> l
            | None ->
              let res, _, _ = Response.of_filename filename in
              res
          in
          let lang, file_loc, gen, cl = Response.parse_input
              ~language:lang (`Raw (filename, lang, contents)) in
          let st = State.set_response_file_loc st file_loc in
          let st = set_lang st lang in
          st, gen_finally gen cl
        | `File file ->
          let loc = {
            Dolmen.Std.Loc.file = State.response_file_loc st;
            loc = Dolmen.Std.Loc.no_loc;
          } in
          let language = State.response_lang st in
          let dir = State.response_dir st in
          begin match Response.find ?language ~dir file with
          | None ->
            let st = State.error ~loc st Parser.file_not_found (dir, file) in
            st, Gen.empty
            | Some file ->
              begin match State.response_mode st with
                | None
                | Some `Incremental ->
                  let lang, file_loc, gen, cl = Response.parse_input ?language (`File file) in
                  let st = State.set_response_file_loc st file_loc in
                  let st = set_lang ~loc st lang in
                  st, gen_finally gen cl
                | Some `Full ->
                  let lang, file_loc, l = Response.parse_file_lazy ?language file in
                  let st = State.set_response_file_loc st file_loc in
                  let st = set_lang ~loc st lang in
                  st, gen_of_llist l
              end
          end
      with
      | Response.Extension_not_found ext ->
        State.error st Parser.extension_not_found ext, Gen.empty
    in
    (* Wrap the resulting parser *)
    st', wrap_parser (Gen.append (Gen.of_list prelude) g)


  (* Typing models *)
  (* ************************************************************************ *)

  let empty_model () =
    let builtins = Dolmen_model.Eval.builtins [
        Dolmen_model.Bool.builtins;
      ] in
    let env = Dolmen_model.Env.empty ~builtins in
    env

  let type_model st ?loc ?attrs l =
    let env = empty_model () in
    List.fold_left (fun (st, env) defs ->
        let st, defs = Typing.defs ~mode:`Use_declared_id st ?loc ?attrs defs in
        let env =
          List.fold_left (fun env def ->
              match def with
              | `Type_def _ -> assert false (* TODO: proper error *)
              | `Term_def (_id, cst, ty_params, term_params, body) ->
                let value =
                  Dolmen_model.Fun.mk
                    ~eval:(Dolmen_model.Eval.eval env)
                    ty_params term_params body
                in
                Dolmen_model.Env.Cst.add cst value env
            ) env defs
        in
        st, env
      ) (st, env) l


  (* Pipe function *)
  (* ************************************************************************ *)

  let get_answer st =
    let t = State.check_state st in
    match t.answers with
    | Response_loaded gen -> gen st
    | Init ->
      let st, gen = parse [] st in
      let answers st =
        let file_loc = State.response_file_loc st in
        let st = State.set_input_file_loc st file_loc in
        match gen st with
        | st, None -> st, None
        | st, Some answer ->
          begin match answer.Dolmen.Std.Answer.descr with
            | Unsat -> st, Some Unsat
            | Sat None -> st, Some (Sat (empty_model ()))
            | Sat Some model ->
              let st, env = type_model st model in
              st, Some (Sat env)
          end
      in
      let st =
        State.set_check_state st
          { t with answers = Response_loaded answers; }
      in
      answers st

  let eval_term env term =
    let value = Dolmen_model.Eval.eval env term in
    match Dolmen_model.Value.extract ~ops:Dolmen_model.Bool.ops value with
    | None -> assert false (* internal failure: wrong sort ? *)
    | Some b -> b

  let eval_hyp env st (_loc, hyp) =
    let res = eval_term env hyp in
    if res then st else assert false (* incorrect model *)

  let eval_goal env st (_loc, goal) =
    let res = eval_term env goal in
    if not res then st else assert false (* incorrect model *)

  let eval_clause env st (_loc, clause) =
    let l = List.map (eval_term env) clause in
    if List.exists Fun.id l then st else assert false (* incorrect model *)

  let check_model st t = function
    | Unsat -> assert false (* cannot check proofs for now *)
    | Sat env ->
      let st = List.fold_left (eval_hyp env) st t.hyps in
      let st = List.fold_left (eval_goal env) st t.goals in
      let st = List.fold_left (eval_clause env) st t.clauses in
      st

  let check st (c : Types.typechecked Types.stmt) =
    let st =
      if State.check_model st then
        let t = State.check_state st in
        let loc = c.loc in
        match c.contents with
        | `Hyp f ->
          State.set_check_state st { t with hyps = (loc, f) :: t.hyps; }
        | `Goal g ->
          State.set_check_state st { t with goals = (loc, g) :: t.goals; }
        | `Clause c ->
          State.set_check_state st { t with clauses = (loc, c) :: t.clauses; }
        | `Solve l ->
          begin match get_answer st with
            | _, None -> assert false
            (* response file does not contain as many answers as check-sats in the original problem  *)
            | st, Some answer ->
              let t = { t with hyps = (List.map (fun f -> (loc, f)) l) @ t.hyps; } in
              let st = check_model st t answer in
              State.set_check_state st { t with hyps = []; goals = []; clauses = []; }
          end
        | `Pop _ | `Push _ | `Reset_assertions | `Reset ->
          assert false
        | _ -> st
      else
        st
    in
    st, c

end
