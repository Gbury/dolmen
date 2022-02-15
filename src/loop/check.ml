
(* Model(/Proof) check *)


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
    (Parse : Parser.Pipe_res
     with type state := State.t)
    (Typing : Typer.S
     with type state := State.t
      and type ty_state := Typer.ty_state
      and type env := Typer.T.env)
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

  (* Typing models *)
  (* ************************************************************************ *)

  let empty_model () =
    let builtins = Dolmen_model.Eval.builtins [
        Dolmen_model.Bool.builtins;
        Dolmen_model.Core.builtins;
      ] in
    let env = Dolmen_model.Env.empty ~builtins in
    env

  let type_model st ~file ?loc ?attrs l =
    let env = empty_model () in
    let input = `Response file in
    List.fold_left (fun (st, env) defs ->
        let st, defs =
          Typing.defs ~mode:`Use_declared_id st ~input ?loc ?attrs defs
        in
        (* Record inferred abstract values *)
        let env =
          List.fold_left (fun env c ->
              let value = Dolmen_model.Abstract.from_cst c in
              Dolmen_model.Env.Cst.add c value env
            ) env (Typing.pop_inferred_model_constants st)
        in
        (* Record the explicit definitions *)
        let env =
          List.fold_left (fun env def ->
              match def with
              | `Type_def _ -> assert false (* TODO: proper error *)
              | `Term_def (_id, cst, ty_params, term_params, body) ->
                let value =
                  Dolmen_model.Fun.mk ~env
                    ~eval:Dolmen_model.Eval.eval
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
      let file = State.response_file st in
      let st, gen = Parse.parse_response [] st file in
      let answers st =
        let file = State.response_file st in
        match gen st with
        | st, None -> st, None
        | st, Some answer ->
          begin match answer.Dolmen.Std.Answer.descr with
            | Unsat -> st, Some Unsat
            | Sat None -> st, Some (Sat (empty_model ()))
            | Sat Some model ->
              let st, env = type_model ~file st model in
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