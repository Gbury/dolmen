
(* Model(/Proof) check *)


(* State *)
(* ************************************************************************ *)

type model = Dolmen_model.Env.t
type term = Dolmen.Std.Expr.Term.t

type answer =
  | Sat of model
  | Unsat of Dolmen.Std.Loc.full

type 'st answers =
  | Init
  | Response_loaded of ('st -> 'st * answer option)

type 't assertion = {
  contents : 't;
  loc : Dolmen.Std.Loc.full;
  file : Logic.language State_intf.file;
}

type 'st t = {
  model : model;
  answers : 'st answers;
  hyps : term assertion list;
  goals : term assertion list;
  clauses : term list assertion list;
}

let empty_model () =
  let builtins = Dolmen_model.Eval.builtins [
      Dolmen_model.Bool.builtins;
      Dolmen_model.Core.builtins;
      Dolmen_model.Array.builtins;
      Dolmen_model.Int.builtins;
    ] in
  let env = Dolmen_model.Env.empty ~builtins in
  env

let empty () = {
  answers = Init;
  model = empty_model ();
  hyps = []; goals = []; clauses = [];
}


(* Warnings and errors *)
(* ************************************************************************ *)

let code = Code.create
    ~category:"Model"
    ~descr:"on model verification errors"

let type_def_in_model =
  Report.Error.mk ~code ~mnemonic:"type-def-in-model"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Type definitions are not allowed in model definitions")
    ~name:"Type definition in Model" ()

let bad_model =
  Report.Error.mk ~code ~mnemonic:"bad-model"
    ~message:(fun fmt kind ->
        match kind with
        | `Hyp ->
          Format.fprintf fmt "This hypothesis/assertion evaluates to false"
        | `Goal ->
          Format.fprintf fmt "This goal evaluates to true"
        | `Clause ->
          Format.fprintf fmt "This assumed clause evaluates to false")
    ~name:"Incorrect model" ()

let cannot_check_proofs =
  Report.Warning.mk ~code ~mnemonic:"check-proof"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Unsat/proofs are not checked")
    ~name:"Cannot check Proofs" ()

let missing_answer =
  Report.Error.mk ~code ~mnemonic:"missing-answer"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "This solve/check-sat statement lacks an answer in the response file")
    ~name:"Missing Answer" ()

let assertion_stack_not_supported =
  Report.Error.mk ~code ~mnemonic:"assertion-stack-in-model"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "The model verification mode does not currently support modifying \
           the assertion stack using push/pop/reset statements")
    ~name:"Assertion Stack in Model" ()

let fo_model =
  Report.Error.mk ~code ~mnemonic:"fo-model"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Models cannot be checked for formulas that contain quantifiers")
    ~name:"First-Order Model" ()


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

  (* Evaluation and errors *)
  (* ************************************************************************ *)

  let eval st ~file ~loc env term =
    try
      Dolmen_model.Eval.eval env term
    with
    | Dolmen_model.Eval.Quantifier ->
      raise (State.Error (State.error st ~file ~loc fo_model ()))

  (* Typing models *)
  (* ************************************************************************ *)

  let record_defs st env ~loc ~(file : _ State_intf.file) typed_defs =
    List.fold_left (fun (st, env) def ->
        match def with
        | `Type_def _ ->
          let st = State.error ~file ~loc st type_def_in_model () in
          st, env
        | `Term_def (_id, cst, ty_params, term_params, body) ->
          let func = Dolmen.Std.Expr.Term.lam (ty_params, term_params) body in
          if State.debug st then
            Format.eprintf "[model][typed][%a] @[<hov>%a := %a@]@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Dolmen.Std.Expr.Term.print func;
          let value = eval st ~file ~loc env func in
          if State.debug st then
            Format.eprintf "[model][value][%a] @[<hov>%a -> %a@]@\n@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Dolmen_model.Value.print value;
          let env = Dolmen_model.Env.Cst.add cst value env in
          st, env
      ) (st, env) typed_defs

  let type_model st ~file ~(loc : Dolmen.Std.Loc.full) ?attrs l =
    let check_st = State.check_state st in
    let env = check_st.model in
    let input = `Response file in
    List.fold_left (fun (st, env) parsed_defs ->
        if State.debug st then
          Format.eprintf "[model][parsed][%a] @[<hov>%a@]@."
            Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
            Dolmen.Std.Statement.(print_group print_def) parsed_defs;
        let st, defs =
          Typing.defs ~mode:`Use_declared_id st ~input ~loc:loc.loc ?attrs parsed_defs
        in
        (* Record inferred abstract values *)
        let env =
          List.fold_left (fun env c ->
              let value = Dolmen_model.Value.abstract_cst c in
              Dolmen_model.Env.Cst.add c value env
            ) env (Typing.pop_inferred_model_constants st)
        in
        (* Record the explicit definitions *)
        record_defs st ~file ~loc env defs
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
          let loc = Dolmen.Std.Loc.{ file = file.loc; loc = answer.loc; } in
          begin match answer.Dolmen.Std.Answer.descr with
            | Unsat ->
              st, Some (Unsat loc)
            | Sat None -> st, Some (Sat (empty_model ()))
            | Sat Some model ->
              let st, env = type_model ~loc ~file st model in
              st, Some (Sat env)
          end
      in
      let st =
        State.set_check_state st
          { t with answers = Response_loaded answers; }
      in
      answers st

  let eval_term st ~file ~loc env term =
    let value = eval st ~file ~loc env term in
    if State.debug st then
      Format.eprintf "[model][eval][%a] @[<hov>%a -> %a@]@\n@."
        Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
        Dolmen.Std.Expr.Term.print term Dolmen_model.Value.print value;
    Dolmen_model.Value.extract_exn ~ops:Dolmen_model.Bool.ops value

  let eval_hyp env st { file; loc; contents = hyp; } =
    let res = eval_term st ~file ~loc env hyp in
    if res then st else
      State.error ~file ~loc st bad_model `Hyp

  let eval_goal env st { file; loc; contents = goal; } =
    let res = eval_term st ~file ~loc env goal in
    if not res then st else
      State.error ~file ~loc st bad_model `Goal

  let eval_clause env st { file; loc; contents = clause; } =
    let l = List.map (eval_term st ~file ~loc env) clause in
    if List.exists Fun.id l then st else
      State.error ~file ~loc st bad_model `Clause

  let check_aux st t = function
    | Unsat loc ->
      let file = State.response_file st in
      State.warn ~file ~loc st cannot_check_proofs ()
    | Sat env ->
      let st = List.fold_left (eval_hyp env) st t.hyps in
      let st = List.fold_left (eval_goal env) st t.goals in
      let st = List.fold_left (eval_clause env) st t.clauses in
      st

  let check st (c : Types.typechecked Types.stmt) =
    let st =
      if State.check_model st then
        let t = State.check_state st in
        let file = State.logic_file st in
        let loc = Dolmen.Std.Loc.{ file = file.loc; loc = c.loc; } in
        match c.contents with
        | #Types.exit
        | #Types.decls
        | #Types.get_info
        | #Types.set_info -> st
        | #Types.stack_control ->
          State.error ~file ~loc st assertion_stack_not_supported ()
        | `Defs defs ->
          let check_st = State.check_state st in
          let st, model = record_defs st check_st.model ~file ~loc defs in
          let st = State.set_check_state st { check_st with model; } in
          st
        | `Hyp contents ->
          let assertion = { file; loc; contents; } in
          State.set_check_state st { t with hyps = assertion :: t.hyps; }
        | `Goal contents ->
          let assertion = { file; loc; contents; } in
          State.set_check_state st { t with goals = assertion :: t.goals; }
        | `Clause contents ->
          let assertion = { file; loc; contents; } in
          State.set_check_state st { t with clauses = assertion :: t.clauses; }
        | `Solve l ->
          begin match get_answer st with
            | _, None ->
              State.error ~file ~loc st missing_answer ()
            | st, Some answer ->
              let local_hyps = List.map (fun contents -> { file; loc; contents; }) l in
              let t = { t with hyps = local_hyps @ t.hyps; } in
              let st = check_aux st t answer in
              State.set_check_state st { t with hyps = []; goals = []; clauses = []; }
          end
      else
        st
    in
    st, c

end
