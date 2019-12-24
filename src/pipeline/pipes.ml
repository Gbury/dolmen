(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

module Make
    (Opt : Options.S)
    (C : Callback.S)
    (Expr : Expr.S)
    (Typer : Typer.S)
    (Solver : Solver.S)
    (Proof : Proof.S)
    (Model : Model.S)
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

  (* Used for wrapping translated contents wiht implicit declarations *)
  type 'a tr_stmt = {
    contents : 'a;
    implicit : Proof.id list;
  }

  (* Typechecked statements *)
  type executed = [ `Executed ]

  type type_defs = [
    | `Type_def of Dolmen.Id.t * Expr.ty_var list * Expr.ty
    | `Term_def of Dolmen.Id.t * Expr.ty_var list * Expr.term_var list * Expr.term
  ]

  type def = [
    | `Def of (Dolmen.Id.t * Proof.term) tr_stmt
  ]

  type type_decls = [
    | `Type_decl of Expr.ty_const
    | `Term_decl of Expr.term_const
  ]

  type decl = [
      `Decl of Proof.id tr_stmt
  ]

  type assume = [
    | `Hyp of Expr.formula
    | `Goal of Expr.formula
    | `Clause of Expr.formula list
  ]

  type solve_sequent = [
    | `Left of Solver.id * Expr.formula
    | `Right of Solver.id * Expr.formula
  ]

  type proof_sequent = [
    | `Left of Proof.id tr_stmt
    | `Right of (Solver.id * Proof.id) tr_stmt
  ]

  type solve = [
    | `Solve of Expr.formula list
  ]

  type result = [
    | `Skipped
    | `Unknown
    | `Proof of Proof.t
    | `Model of Model.t
  ]

  (* Agregate types *)
  type typechecked = [ executed | type_defs | type_decls | assume | solve ]
  type solved      = [ executed | type_defs | type_decls | solve_sequent | result ]
  type translated  = [ executed | decl | def | proof_sequent | result ]

  (* Simple constructor *)
  (* let tr implicit contents = { implicit; contents; } *)
  (* let simple id loc contents = { id; loc; contents; } *)

  (* Parsing *)
  (* ************************************************************************ *)

  let wrap_parser g = fun opt ->
    if Opt.is_interactive opt then
      Format.printf "%s@?" (Opt.prelude opt);
    C.start `Parsing;
    let ret = g () in
    C.stop `Parsing;
    ret

  let parse prelude opt =
    C.start `Parsing;
    (* Parse the input *)
    let opt', g =
      match Opt.input opt with
      | `Stdin ->
        let lang, gen, _ = Parse.parse_input
            ?language:(Opt.lang opt) (`Stdin Parse.Smtlib) in
        Opt.set_lang opt lang, gen
      | `File f ->
        let s = Dolmen.Statement.include_ f [] in
        (* Auto-detect input format *)
        let lang =
          match Opt.lang opt with
          | Some l -> l
          | None ->
            let res, _, _ = Parse.of_filename f in
            res
        in
        (* Formats Dimacs and Tptp are descriptive and lack the emission
            of formal solve/prove instructions, so we need to add them. *)
        let s' =
          match lang with
          | Parse.Zf
          | Parse.ICNF
          | Parse.Smtlib -> s
          | Parse.Dimacs
          | Parse.Tptp ->
            Dolmen.Statement.pack [s; Dolmen.Statement.prove ()]
        in
        Opt.set_lang opt lang,
        (Gen.singleton s')
    in
    C.stop `Parsing;
    (* Wrap the resulting parser *)
    opt', wrap_parser (Gen.append (Gen.of_list prelude) g)

  (* Execute statements *)
  (* ************************************************************************ *)

  (* let none = Dolmen.Id.mk Dolmen.Id.decl "<>" *)

  let execute (opt, c) =
    match c with
    (* Exit the prover, no need to return a statement. *)
    | { S.descr = S.Exit; _ } -> exit 0
    (* TODO: parse and apply option changes *)
    (* TODO: print model, proofs etc.. according to smtlib standard *)
    | _ -> `Continue (opt, c)

  (* Expand dolmen statements *)
  (* ************************************************************************ *)
(*
  let expand (opt, c) =
    C.start `Include;
    let ret = match c with
      | { S.descr = S.Pack l; _ } ->
        opt, `Gen (true, Gen.of_list l)
      (* TODO: filter the statements by passing some options *)
      | { S.descr = S.Include f; _ } ->
        let language = Opt.lang opt in
        let dir = Options.(opt.input.dir) in
        begin
          match In.find ?language ~dir f with
          | None -> raise (Options.File_not_found f)
          | Some file ->
            (* TODO: cleanup files after having read them ?
               only useful if there happens to be a very long long
               (i.e. at least a few thousands) chain of nested includes) *)
            let l, gen, _ = In.parse_input ?language (`File file) in
            let opt' = Options.({
                opt with input = {
                opt.input with format = Some l;
                               file = `File file;
                               mode = Regular }
              } ) in
            opt', `Gen (false, gen)
        end
      | _ -> (opt, `Ok)
    in
    Util.exit_prof expand_section;
    ret


  (* Typechecking *)
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

  (* TODO, unwind backtrak stack on exceptions ? *)
  let type_wrap ?(goal=false) opt =
    let l = CCOpt.get_exn Options.(opt.input.format) in
    let status =
      if goal then Expr.Status.goal
      else Expr.Status.hypothesis
    in
    let explain = Options.(opt.typing.explain) in
    let expect =
      if Options.(opt.typing.infer) then
        Type.Typed Expr.Ty.prop
      else match Options.(opt.input.format) with
        | Some In.Tptp -> Type.Typed Expr.Ty.prop
        | Some In.Dimacs -> Type.Typed Expr.Ty.prop
        | _ -> Type.Nothing
    in
    let infer_hook env = function
      | Type.Ty_fun _ -> ()
      | Type.Term_fun cst -> Synth.add_id cst
    in
    let env = Type.empty_env
        ~status ~explain ~expect ~infer_hook
        (Semantics.type_env l)
    in
    env

  let add_synth = function
    | `Term_decl cst -> Synth.add_id cst
    | _ -> ()

  let run_typecheck opt = Options.(opt.typing.typing)

  let fv_list l =
    let l' = List.map Dolmen.Term.fv l in
    CCList.sort_uniq Dolmen.Id.compare (List.flatten l')

  let typecheck (opt, c) : typechecked stmt =
    Util.enter_prof Type.section;
    let res =
      match c with
      (* Declarations and definitions *)
      | { S.descr = S.Def (id, t) } ->
        start_section ~section:Type.section Util.debug "Definition";
        let env = type_wrap opt in
        let ret = Type.new_def env t ?attr:c.S.attr id in
        (simple (def_id c) c.S.loc ret :> typechecked stmt)
      | { S.descr = S.Decl (id, t) } ->
        start_section ~section:Type.section Util.debug "Declaration typing";
        let env = type_wrap opt in
        let ret = Type.new_decl env t ?attr:c.S.attr id in
        let () = add_synth ret in
        (simple (decl_id c) c.S.loc ret :> typechecked stmt)
      (* Hyps and goal statements *)
      | { S.descr = S.Prove l } ->
        start_section ~section:Type.section Util.debug "Assumption typing";
        let env = type_wrap opt in
        let l' = List.map (Type.new_formula env) l in
        simple (prove_id c) c.S.loc (`Solve l')
      | { S.descr = S.Clause l } ->
        start_section ~section:Type.section Util.debug "Clause typing";
        let env = type_wrap opt in
        begin match fv_list l with
          | [] -> (* regular clauses *)
            let res = List.map (Type.new_formula env) l in
            (simple (hyp_id c) c.S.loc (`Clause res) :> typechecked stmt)
          | free_vars -> (* if there are free variables, these must be quantified
                            or else the typchecker will raise an error. *)
            let loc = c.S.loc in
            let vars = List.map (Dolmen.Term.const ?loc) free_vars in
            let f = Dolmen.Term.forall ?loc vars (
                match l with
                | [] -> assert false | [p] -> p
                | _ -> Dolmen.Term.apply ?loc (Dolmen.Term.or_t ?loc ()) l
              ) in
            let res = Type.new_formula env f in
            (simple (hyp_id c) c.S.loc (`Hyp res) :> typechecked stmt)
        end
      | { S.descr = S.Antecedent t } ->
        start_section ~section:Type.section Util.debug "Hypothesis typing";
        let env = type_wrap opt in
        let ret = Type.new_formula env t in
        (simple (hyp_id c) c.S.loc (`Hyp ret) :> typechecked stmt)
      | { S.descr = S.Consequent t } ->
        start_section ~section:Type.section Util.debug "Goal typing";
        let env = type_wrap ~goal:true opt in
        let ret = Type.new_formula env t in
        (simple (goal_id c) c.S.loc (`Goal ret) :> typechecked stmt)
      (* We can safely ignore set-logic "dimacs", as it only gives the number of atoms
          and clauses of the dimacs problem, which is of no interest. *)
      | { S.descr = S.Set_logic "dimacs" } ->
        simple none c.S.loc `Executed
      (* Other set_logics should check whether corresponding plugins are activated ? *)
      | { S.descr = S.Set_logic _ } -> simple none c.S.loc `Executed
      (* Set info can always be ignored. *)
      | { S.descr = S.Set_info _ } -> simple none c.S.loc `Executed

      (* Other untreated statements *)
      | _ -> raise (Options.Stmt_not_implemented c)
    in
    Util.exit_prof Type.section;
    res

  (* Solving *)
  (* ************************************************************************ *)

  let solve (opt, (c : typechecked stmt)) : solved stmt =
    Util.enter_prof Solver.section;
    let res =
      match c with
      | ({contents = `Executed; _ } as res)
      | ({ contents = `Type_def _; _ } as res)
      | ({ contents = `Term_def _; _ } as res)
      | ({ contents = `Type_decl _; _ } as res)
      | ({ contents = `Term_decl _; _ } as res) ->
        res
      | ({ contents = `Clause l; _ } as res) ->
        start_section ~section:Dispatcher.section Util.debug "Assume clause";
        let id = Solver.assume ~solve:Options.(opt.solve) l in
        let f = match l with
          | [] -> assert false | [p] -> p
          | _ -> Expr.Formula.f_or l
        in
        (simple res.id res.loc (`Left (id, f)) :> solved stmt)
      | ({ contents = `Hyp f; _ } as res) ->
        start_section ~section:Dispatcher.section Util.debug "Assume hyp";
        let id = Solver.assume ~solve:Options.(opt.solve) [f] in
        (simple res.id res.loc (`Left (id, f)) :> solved stmt)
      | ({ contents = `Goal f; _ } as res) ->
        start_section ~section:Dispatcher.section Util.info "Assume goal";
        let id = Solver.assume ~solve:Options.(opt.solve)
            [Expr.Formula.neg ~status:Expr.Status.goal f] in
        (simple res.id res.loc (`Right (id, f)) :> solved stmt)
      | { contents = `Solve assumptions; _ } ->
        let ret =
          if opt.Options.solve then begin
            start_section ~section:Dispatcher.section Util.log "Solve";
            let check_model = Options.(opt.model.active) in
            let check_proof = Options.(opt.proof.active) in
            let export = Options.(opt.output.icnf) in
            begin match Solver.solve ~check_model ~check_proof ~assumptions ?export () with
              | Solver.Sat m -> `Model m
              | Solver.Unsat p -> `Proof p
              | Solver.Unknown -> `Unknown
            end
          end else
            `Unknown
        in
        { c with contents = ret }
    in
    Util.exit_prof Solver.section;
    res

  (* Printing results *)
  (* ************************************************************************ *)

  let print_res (opt, (c : solved stmt)) =
    match c with
    | { contents = `Executed; _ }
    | { contents = `Type_def _; _ }
    | { contents = `Term_def _; _ }
    | { contents = `Type_decl _; _ }
    | { contents = `Term_decl _; _ }
    | { contents = `Left _; _ }
    | { contents = `Right _; _ }
    | { contents = `Skipped; _ } ->
      ()
    | { contents = `Model _; _ } ->
      Util.printf "%a@." Out.print_sat opt
    | { contents = `Proof _; _ } ->
      Util.printf "%a@." Out.print_unsat opt
    | { contents = `Unknown; _ } ->
      Util.printf "%a@." Out.print_unknown opt

  (* Translate terms to proof terms *)
  (* ************************************************************************ *)

  (* Wrapper to get implicitly typed identifiers. *)
  let mk_callback () =
    let l = ref [] in
    let callback id =
      Util.debug "Found implicitly typed constant: %a"
        Expr.Id.print id;
      l := id :: !l
    in
    (fun () -> List.rev !l), callback

  let run_translate opt = Options.(opt.translate)

  let translate (opt, (c : solved stmt)) =
    Term.clean_traps ();
    match c with
    | ({ contents = `Executed; _ } as res)
    | ({ contents = `Skipped; _ } as res)
    | ({ contents = `Unknown; _ } as res)
    | ({ contents = `Model _; _ } as res)
    | ({ contents = `Proof _; _ } as res) ->
      (res :> translated stmt)

    | { contents = `Type_def (id, vars, res); _ } ->
      let get, callback = mk_callback () in
      let t_vars = List.map (Term.of_id_aux ~kind:`Var ~callback Term.of_ttype) vars in
      let t = Term.lambdas t_vars (Term.of_ty ~callback res) in
      (simple c.id c.loc @@ `Def (tr (get ()) (id, t)) :> translated stmt)

    | { contents = `Term_def (id, vars, args, res); _ } ->
      let get, callback = mk_callback () in
      let t_vars = List.map (Term.of_id_aux ~kind:`Var ~callback Term.of_ttype) vars in
      let t_args = List.map (Term.of_id_aux ~kind:`Var ~callback Term.of_ty) args in
      let t = Term.lambdas (t_vars @ t_args) (Term.of_term ~callback res) in
      (simple c.id c.loc @@ `Def (tr (get ()) (id, t)) :> translated stmt)

    | { contents = `Type_decl f; _ } ->
      let get, callback = mk_callback () in
      let id = Term.of_id_aux ~kind:`Declared ~callback
          (Term.of_function_descr Term.of_unit Term.of_ttype) f
      in
      (simple c.id c.loc @@ `Decl (tr (get ()) id) :> translated stmt)

    | { contents = `Term_decl f; _ } ->
      let get, callback = mk_callback () in
      let id = Term.of_id_aux ~kind:`Declared ~callback
          (Term.of_function_descr Term.of_ttype Term.of_ty) f
      in
      (simple c.id c.loc @@ `Decl (tr (get ()) id) :> translated stmt)

    | { contents = `Left (sid, f); id; _ } ->
      let get, callback = mk_callback () in
      let t = Term.of_formula ~callback f in
      let p = Term.declare (Dolmen.Id.full_name id) t in
      let () = Solver.register_hyp sid p in
      (simple c.id c.loc @@ `Left (tr (get ()) p) :> translated stmt)

    | { contents = `Right (sid, f); id; _ } ->
      let get, callback = mk_callback () in
      let t = Term.of_formula ~callback f in
      let p = Term.declare (Dolmen.Id.full_name id) t in
      (simple c.id c.loc @@ `Right (tr (get ()) (sid, p)) :> translated stmt)


  (* Export information *)
  (* ************************************************************************ *)

  (* TODO: export section *)
  let export (opt, (c : translated stmt)) =
    match c with
    | { contents = `Executed; _ }
    | { contents = `Def _; _ } ->
      ()
    | { contents = `Decl { contents = id ; implicit }; _ } ->
      Export.declare_id ?loc:c.loc opt implicit (c.id, id)
    | { contents = `Left { contents = id; implicit }; _ } ->
      Export.declare_hyp ?loc:c.loc opt implicit id
    | { contents = `Right { contents = (_, id); implicit }; _ } ->
      Export.declare_goal ?loc:c.loc opt implicit id
    | { contents = `Skipped; _ } ->
      Export.declare_solve ?loc:c.loc opt ()
    | { contents = `Unknown; _ }
    | { contents = `Model _; _ }
    | { contents = `Proof _; _ } ->
      pp_opt Solver.export_dimacs Options.(opt.output.dimacs) ();
      Export.declare_solve ?loc:c.loc opt ()

  (* Printing proofs *)
  (* ************************************************************************ *)

  let print_proof (opt, (c : translated stmt)) =
    Util.enter_prof Proof.section;
    begin match c with
      (* Not much to do with these... *)
      | { contents = `Executed; _ }
      | { contents = `Def _; _ } -> ()
      | { contents = `Skipped; _ } ->
        if Options.(opt.proof.active) then
          Util.warn "Proof check/output activated, but solving is deactivated"
      | { contents = `Model _; _ } ->
        if Options.(opt.proof.active) then
          Util.warn "Proof check/output activated, but a model was found"
      | { contents = `Unknown; _ } ->
        if Options.(opt.proof.active) then
          Util.warn "Proof check/output activated, but no proof was found"

      (* Interesting parts *)
      | { contents = `Decl { implicit; contents; }; _ } ->
        Prove.declare_id ?loc:c.loc Options.(opt.proof) implicit contents
      | { contents = `Left { implicit; contents = p }; id; _ } ->
        Prove.declare_hyp ?loc:c.loc Options.(opt.proof) id implicit p
      | { contents = `Right { implicit; contents = (sid, p) }; id; _ } ->
        Prove.declare_goal ?loc:c.loc Options.(opt.proof) id implicit (sid, p)
      | { contents = `Proof p; _ } ->
        Util.info "Resolution proof size: %a" Util.print_size (Util.size p);
        Prove.output_proof Options.(opt.proof) p
    end;
    Util.exit_prof Proof.section

  (* Printing models *)
  (* ************************************************************************ *)

  let print_model (opt, (c : translated stmt)) =
    match c with
    | { contents = `Executed; _ }
    | { contents = `Def _; _ }
    | { contents = `Decl _; _ }
    | { contents = `Left _; _ }
    | { contents = `Right _ } -> ()
    | { contents = `Skipped; _ } ->
      if Options.(opt.model.active) then
        Util.warn "Model check/output activated, but solving is deactivated"
    | { contents = `Proof _; _ } ->
      if Options.(opt.model.active) then
        Util.warn "Model check/output activated, but a proof was found"
    | { contents = `Unknown; _ } ->
      if Options.(opt.model.active) then
        Util.warn "Model check/output activated, but no model was found"

    (* Interesting parts *)
    | { contents = `Model m; _ } ->
      Util.info "Model size: %a" Util.print_size (Util.size m);
      pp_opt Solver.Model.print Options.(opt.model.assign) m
*)
end
