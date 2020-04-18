(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

module Make
    (Expr : Expr_intf.S)
    (State : State_intf.S
     with type term := Expr.term)
    (Typer : Typer_intf.S
     with type state := State.t
      and type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_const := Expr.ty_const
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_const := Expr.term_const
      and type formula := Expr.formula)
= struct

  (* Module alias & Helper functions *)
  (* ************************************************************************ *)

  module S = Dolmen.Statement

  (* Types used in Pipes *)
  (* ************************************************************************ *)

  (* Used for representing typed statements *)
  type +'a stmt = {
    id : Dolmen.Id.t;
    contents  : 'a;
    loc : Dolmen.ParseLocation.t option;
  }

  type defs = [
    | `Type_def of Dolmen.Id.t * Expr.ty_var list * Expr.ty
    | `Term_def of Dolmen.Id.t * Expr.ty_var list * Expr.term_var list * Expr.term
  ]

  type decl = [
    | `Type_decl of Expr.ty_const
    | `Term_decl of Expr.term_const
  ]

  type decls = [
    | `Decls of decl list
  ]

  type assume = [
    | `Hyp of Expr.formula
    | `Goal of Expr.formula
    | `Clause of Expr.formula list
  ]

  type solve = [
    | `Solve of Expr.formula list
  ]

  type get_info = [
    | `Get_info of string
    | `Get_option of string
    | `Get_proof
    | `Get_unsat_core
    | `Get_unsat_assumptions
    | `Get_model
    | `Get_value of Expr.term list
    | `Get_assignment
    | `Get_assertions
    | `Echo of string
    | `Plain of Dolmen.Statement.term
  ]

  type set_info = [
    | `Set_info of Dolmen.Statement.term
    | `Set_option of Dolmen.Statement.term
  ]

  type stack_control = [
    | `Pop of int
    | `Push of int
    | `Reset_assertions
    | `Reset
    | `Exit
  ]

  (* Agregate types *)
  type typechecked = [ defs | decls | assume | solve | get_info | set_info | stack_control ]

  (* Simple constructor *)
  (* let tr implicit contents = { implicit; contents; } *)
  let simple id loc (contents: typechecked)  = { id; loc; contents; }

  (* Parsing *)
  (* ************************************************************************ *)

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
    in
    aux

  let wrap_parser g = fun st ->
    if State.is_interactive st then
      Format.printf "%s @?" (State.prelude st);
    State.start `Parsing;
    let ret = g () in
    State.stop `Parsing;
    ret

  let parse prelude st =
    State.start `Parsing;
    (* Parse the input *)
    let st', g =
      match State.input_source st with
      | `Stdin ->
        let lang, gen, _ = Parser.parse_input
            ?language:(State.input_lang st)
            (`Stdin (Parser.Smtlib2 `Latest))
        in
        State.set_lang st lang, gen
      | `Raw (filename, contents) ->
        let lang =
          match State.input_lang st with
          | Some l -> l
          | None ->
            let res, _, _ = Parser.of_filename filename in
            res
        in
        let lang, gen, cl = Parser.parse_input
            ~language:lang (`Raw (filename, lang, contents)) in
        State.set_lang st lang, gen_finally gen cl
      | `File f ->
        let s = Dolmen.Statement.include_ f [] in
        (* Auto-detect input format *)
        let lang =
          match State.input_lang st with
          | Some l -> l
          | None ->
            let res, _, _ = Parser.of_filename f in
            res
        in
        (* Formats Dimacs and Tptp are descriptive and lack the emission
            of formal solve/prove instructions, so we need to add them. *)
        let s' =
          match lang with
          | Parser.Zf
          | Parser.ICNF
          | Parser.Smtlib2 _
          | Parser.Alt_ergo -> s
          | Parser.Dimacs
          | Parser.Tptp _ ->
            Dolmen.Statement.pack [s; Dolmen.Statement.prove ()]
        in
        State.set_lang st lang,
        (Gen.singleton s')
    in
    State.stop `Parsing;
    (* Wrap the resulting parser *)
    st', wrap_parser (Gen.append (Gen.of_list prelude) g)

  (* Expand dolmen statements *)
  (* ************************************************************************ *)

  let gen_of_list l =
    let l = ref l in
    (fun () -> match !l with
       | [] -> None
       | x :: r -> l := r; Some x
    )

  let expand (st, c) =
    State.start `Include;
    let ret = match c with
      | { S.descr = S.Pack l; _ } ->
        st, `Gen (true, Gen.of_list l)
      (* TODO: filter the statements by passing some stions *)
      | { S.descr = S.Include file; _ } ->
        let loc = c.loc in
        let language = State.input_lang st in
        let dir = State.input_dir st in
        begin
          match Parser.find ?language ~dir file with
          | None ->
            State.file_not_found ?loc ~dir ~file
          | Some file ->
            begin match State.input_mode st with
              | None
              | Some `Incremental ->
                let lang, gen, cl = Parser.parse_input ?language (`File file) in
                let st = State.set_lang st lang in
                st, `Gen (true, gen_finally gen cl)
              | Some `Full ->
                let lang, l = Parser.parse_file ?language file in
                let st = State.set_lang st lang in
                st, `Gen (true, gen_of_list l)
            end
        end
      | _ -> (st, `Ok)
    in
    State.stop `Include;
    ret


  (* Typechecking & Statement execution *)
  (* ************************************************************************ *)

  let stmt_id ref_name =
    let counter = ref 0 in
    (fun c ->
       match c.Dolmen.Statement.id with
       | { Dolmen.Id.ns = Dolmen.Id.Decl; name = "" } ->
         let () = incr counter in
         let name = Format.sprintf "%s_%d" ref_name !counter in
         Dolmen.Id.mk Dolmen.Id.decl name
       | id -> id)

  let def_id   = stmt_id "def"
  let decl_id  = stmt_id "decl"
  let hyp_id   = stmt_id "hyp"
  let goal_id  = stmt_id "goal"
  let prove_id = stmt_id "prove"
  let other_id = stmt_id "other"

  let fv_list l =
    let l' = List.map Dolmen.Term.fv l in
    List.sort_uniq Dolmen.Id.compare (List.flatten l')

  let add_warnings st l =
    List.fold_left (fun st (loc, msg) ->
        State.warn st loc msg
      ) st l

  let typecheck (st, c) =
    State.start `Typing;
    let res =
      if not (Typer.typecheck st) then
        `Done st
      else match c with

      (* Pack and includes.
         These should have been filtered out before this point.
         TODO: emit some kind of warning ? *)
      | { S.descr = S.Pack _; _ } -> `Done st
      | { S.descr = S.Include _; _ } -> `Done st

      (* Assertion stack Management *)
      | { S.descr = S.Pop i; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Pop i))
      | { S.descr = S.Push i; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Push i))
      | { S.descr = S.Reset_assertions; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Reset_assertions)

      (* Plain statements
         TODO: allow the `plain` function to return a meaningful value *)
      | { S.descr = S.Plain t; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Plain t))

      (* Hypotheses and goal statements *)
      | { S.descr = S.Prove l; _ } ->
        let st, l, w = Typer.formulas st ?attr:c.S.attr l in
        let st = add_warnings st w in
        `Continue (st, simple (prove_id c) c.S.loc (`Solve l))

      (* Hypotheses & Goals *)
      | { S.descr = S.Clause l; _ } ->
        begin match fv_list l with
          | [] -> (* regular clauses *)
            let st, res, w = Typer.formulas st ?attr:c.S.attr l in
            let st = add_warnings st w in
            let stmt : typechecked stmt =
              simple (hyp_id c) c.S.loc (`Clause res)
            in
            `Continue (st, stmt)
          | free_vars -> (* if there are free variables, these must be quantified
                            or else the typchecker will raise an error. *)
            let loc = c.S.loc in
            let vars = List.map (Dolmen.Term.const ?loc) free_vars in
            let f = Dolmen.Term.forall ?loc vars (
                match l with
                | [] -> assert false
                | [p] -> p
                | _ -> Dolmen.Term.apply ?loc (Dolmen.Term.or_t ?loc ()) l
              ) in
            let st, res, w = Typer.formula st ?attr:c.S.attr ~goal:false f in
            let st = add_warnings st w in
            let stmt : typechecked stmt =
              simple (hyp_id c) c.S.loc (`Hyp res)
            in
            `Continue (st, stmt)
        end
      | { S.descr = S.Antecedent t; _ } ->
        let st, ret, w = Typer.formula st ?attr:c.S.attr ~goal:false t in
        let st = add_warnings st w in
        let stmt : typechecked stmt =
          simple (hyp_id c) c.S.loc (`Hyp ret)
        in
        `Continue (st, stmt)
      | { S.descr = S.Consequent t; _ } ->
        let st, ret, w = Typer.formula st ?attr:c.S.attr ~goal:true t in
        let st = add_warnings st w in
        let stmt : typechecked stmt =
          simple (goal_id c) c.S.loc (`Goal ret)
        in
        `Continue (st, stmt)

      (* We can safely ignore set-logic "dimacs", as it only gives the number of atoms
          and clauses of the dimacs problem, which is of no interest. *)
      | { S.descr = S.Set_logic "dimacs"; _ } ->
        `Done st
      (* Other set_logics should check whether corresponding plugins are activated ? *)
      | { S.descr = S.Set_logic s; _ } ->
        let st = State.set_logic st s in
        `Done st

      (* Set/Get info *)
      | { S.descr = S.Get_info s; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Get_info s))
      | { S.descr = S.Set_info t; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Set_info t))

      (* Set/Get options *)
      | { S.descr = S.Get_option s; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Get_option s))
      | { S.descr = S.Set_option t; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Set_option t))

      (* Declarations and definitions *)
      | { S.descr = S.Def (id, t); _ } ->
        let st, ret, w = Typer.def st ?attr:c.S.attr id t in
        let st = add_warnings st w in
        `Continue (st, (simple (def_id c) c.S.loc (ret :> typechecked)))
      | { S.descr = S.Decls l; _ } ->
        let st, l, w = Typer.decls st ?attr:c.S.attr l in
        let st = add_warnings st w in
        let res : typechecked stmt = simple (decl_id c) c.S.loc (`Decls l) in
        `Continue (st, res)

      (* Smtlib's proof/model instructions *)
      | { S.descr = S.Get_proof; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Get_proof)
      | { S.descr = S.Get_unsat_core; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Get_unsat_core)
      | { S.descr = S.Get_unsat_assumptions; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Get_unsat_assumptions)
      | { S.descr = S.Get_model; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Get_model)
      | { S.descr = S.Get_value l; _ } ->
        let st, l, w = Typer.terms st ?attr:c.S.attr l in
        let st = add_warnings st w in
        `Continue (st, simple (other_id c) c.S.loc (`Get_value l))
      | { S.descr = S.Get_assignment; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Get_assignment)
      (* Assertions *)
      | { S.descr = S.Get_assertions; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Get_assertions)
      (* Misc *)
      | { S.descr = S.Echo s; _ } ->
        `Continue (st, simple (other_id c) c.S.loc (`Echo s))
      | { S.descr = S.Reset; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Reset)
      | { S.descr = S.Exit; _ } ->
        `Continue (st, simple (other_id c) c.S.loc `Exit)

    in
    State.stop `Typing;
    res
end
