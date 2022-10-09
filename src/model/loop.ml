
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Model(/Proof) check *)

(* State *)
(* ************************************************************************ *)

type model = Model.t
type cst = Dolmen.Std.Expr.term_cst
type term = Dolmen.Std.Expr.Term.t

type answer =
  | Sat
  | Unsat of Dolmen.Std.Loc.full
  | Error of Dolmen.Std.Loc.full

type 'st answers =
  | Init
  | Response_loaded of ('st -> 'st * answer option)

type 't located = {
  contents : 't;
  loc : Dolmen.Std.Loc.full;
  file : Dolmen_loop.Logic.language Dolmen_loop.State.file;
}

type 'st t = {
  model : model;
  answers : 'st answers;
  defs : (cst * term) list located list;
  hyps : term located list;
  goals : term located list;
  clauses : term list located list;
}

let empty () = {
  answers = Init;
  model = Model.empty;
  defs = []; hyps = []; goals = []; clauses = [];
}


(* Warnings and errors *)
(* ************************************************************************ *)

let pp_wrap pp fmt x =
  Format.fprintf fmt "`%a`" pp x

let code = Dolmen_loop.Code.create
    ~category:"Model"
    ~descr:"on model verification errors"

let type_def_in_model =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"type-def-in-model"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Type definitions are not allowed in model definitions")
    ~name:"Type definition in Model" ()

let bad_model =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"bad-model"
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
  Dolmen_loop.Report.Warning.mk ~code ~mnemonic:"check-proof"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Unsat/proofs are not checked")
    ~name:"Cannot check Proofs" ()

let error_in_response =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"response-error"
    ~message:(fun fmt () ->
        Format.fprintf fmt "@[<hov>%a@]" Format.pp_print_text
          "The response contains an error, whereas a \
           sat/unsat answer was expected")
    ~name:"Error in Response" ()

let missing_answer =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"missing-answer"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "This solve/check-sat statement lacks an answer in the response file")
    ~name:"Missing Answer" ()

let assertion_stack_not_supported =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"assertion-stack-in-model"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "The model verification mode does not currently support modifying \
           the assertion stack using push/pop/reset statements")
    ~name:"Assertion Stack in Model" ()

let fo_model =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"fo-model"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Models cannot be checked for formulas that contain quantifiers")
    ~name:"First-Order Model" ()

let undefined_variable =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"undefined-variable"
    ~message:(fun fmt v ->
        Format.fprintf fmt
          "The following variable is not defined/let-bound, and thus
          has no value: %a"
          (pp_wrap Dolmen.Std.Expr.Term.Var.print) v)
      ~name:"Undefined variable in Model verification" ()

let undefined_constant =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"undefined-constant"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The following constant is not defined, and thus
          has no value: %a"
          (pp_wrap Dolmen.Std.Expr.Term.Const.print) c)
      ~name:"Undefined variable in Model verification" ()

let unhandled_builtin =
  Dolmen_loop.Report.Error.mk ~code:Dolmen_loop.Code.bug
    ~mnemonic:"unhandled-builtin-model"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The following constant is currently not handled during \
           model verification@ %a.@ Please report upstream"
          (pp_wrap Dolmen.Std.Expr.Term.Const.print) c)
      ~name:"Unhandled builtin in Model verification" ()

let partial_destructor =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"partial-dstr"
    ~message:(fun fmt (cstr, value) ->
        Format.fprintf fmt
          "Partial destructr: the destructor for constructor %a \
           was applied to the following value: %a"
          (pp_wrap Dolmen.Std.Expr.Term.Const.print) cstr
          (pp_wrap Value.print) value)
    ~name:"Partial Destructor" ()

(* Pipe *)
(* ************************************************************************ *)

type 'a file = 'a Dolmen_loop.State.file

module Make
    (State : Dolmen_loop.State.S)
    (Parse : Dolmen_loop.Parser.S
     with type state := State.t
      and type 'a key := 'a State.key)
    (Typer : Dolmen_loop.Typer.Typer_Full
     with type state := State.t
      and type 'a key := 'a State.key
      and type ty_state := Dolmen_loop.Typer.ty_state
      and type env := Dolmen_loop.Typer.T.env)
    (Typer_Pipe : Dolmen_loop.Typer.S
     with type state := State.t
      and type 'a key := 'a State.key
      and type ty := Dolmen.Std.Expr.ty
      and type ty_var := Dolmen.Std.Expr.ty_var
      and type ty_cst := Dolmen.Std.Expr.ty_cst
      and type term := Dolmen.Std.Expr.term
      and type term_var := Dolmen.Std.Expr.term_var
      and type term_cst := Dolmen.Std.Expr.term_cst
      and type formula := Dolmen.Std.Expr.formula)
= struct

  let pipe = "Model"
  let check_model = Typer.check_model
  let check_state = State.create_key ~pipe "check_state"

  let init
      ~check_model:check_model_value
      ?check_state:(check_state_value=empty ())
      st =
    st
    |> State.set check_model check_model_value
    |> State.set check_state check_state_value

  (* Evaluation and errors *)
  (* ************************************************************************ *)

    let builtins =
      Eval.builtins [
        Adt.builtins;
        Bool.builtins;
        Core.builtins;
        Array.builtins;
        Int.builtins;
        Rat.builtins;
        Real.builtins;
        Bitv.builtins;
        Fp.builtins;
        Coercion.builtins;
      ]

  let eval st ~file ~loc term =
    let _err err args =
      raise (State.Error (State.error st ~file ~loc err args))
    in
    let { model; _ } = State.get check_state st in
    let env = Env.mk model ~builtins in
    try
      Eval.eval env term
    with
    | Eval.Quantifier -> _err fo_model ()
    | Eval.Unhandled_builtin b -> _err unhandled_builtin b
    | Eval.Undefined_variable v -> _err undefined_variable v
    | Eval.Undefined_constant c -> _err undefined_constant c
    | Adt.Partial_destructor (cstr, value) -> _err partial_destructor (cstr, value)

  (* Typing models *)
  (* ************************************************************************ *)

  let define_value st cst value =
    let t = State.get check_state st in
    let model = Model.Cst.add cst value t.model in
    State.set check_state { t with model; } st

  let corner_2 = function
    | [ `Term_def (_id, _cst, [], [x; y], body) ] ->
      Some (fun env a b ->
          let env = Env.update_model env (Model.Var.add x a) in
          let env = Env.update_model env (Model.Var.add y b) in
          Eval.eval env body
        )
    | _ -> assert false (* TODO: raise proper error *)

  let pack_abstract_defs ~loc ~(file:  _ file) typed_defs =
    let contents =
      List.filter_map (function
          | `Type_def _ -> None
          | `Term_def (_id, cst, ty_params, term_params, body) ->
            let func = Dolmen.Std.Expr.Term.lam (ty_params, term_params) body in
            Some (cst, func)
        ) typed_defs
    in
    { contents; loc; file; }

  let record_defs st ~loc ~(file : _ file) typed_defs =
    List.fold_left (fun st def ->
        match def with
        | `Type_def _ ->
          State.error ~file ~loc st type_def_in_model ()
        | `Term_def (_id, cst, ty_params, term_params, body) ->
          let func = Dolmen.Std.Expr.Term.lam (ty_params, term_params) body in
          if State.get State.debug st then
            Format.eprintf "[model][typed][%a] @[<hov>%a := %a@]@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Dolmen.Std.Expr.Term.print func;
          let value = eval st ~file ~loc func in
          if State.get State.debug st then
            Format.eprintf "[model][value][%a] @[<hov>%a -> %a@]@\n@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Value.print value;
          define_value st cst value
      ) st typed_defs

  let type_model st ~file ~(loc : Dolmen.Std.Loc.full) ?attrs l =
    let input = `Response file in
    let st = Typer.push ~input st 1 in
    let st =
      List.fold_left (fun st parsed_defs ->
          if State.get State.debug st then
            Format.eprintf "[model][parsed][%a] @[<hov>%a@]@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Statement.(print_group print_def) parsed_defs;
          let st, defs =
            Typer.defs ~mode:`Use_declared_id st ~input ~loc:loc.loc ?attrs parsed_defs
          in
          (* Record inferred abstract values *)
          let st =
            List.fold_left (fun st c ->
                let value = Value.abstract_cst c in
                define_value st c value
              ) st (Typer.pop_inferred_model_constants st)
          in
          (* Record the explicit definitions *)
          record_defs st ~file ~loc defs
        ) st l
    in
    let st = Typer.pop st ~input 1 in
    st


  (* Pipe function *)
  (* ************************************************************************ *)

  let get_answer st =
    let t = State.get check_state st in
    match t.answers with
    | Response_loaded gen -> gen st
    | Init ->
      let file = State.get State.response_file st in
      let st, gen = Parse.parse_response [] st file in
      let answers st =
        let file = State.get State.response_file st in
        match gen st with
        | st, None -> st, None
        | st, Some answer ->
          let loc = Dolmen.Std.Loc.{ file = file.loc; loc = answer.loc; } in
          begin match answer.Dolmen.Std.Answer.descr with
            | Unsat -> st, Some (Unsat loc)
            | Error _ -> st, Some (Error loc)
            | Sat None -> st, Some Sat
            | Sat Some model ->
              let st = type_model ~loc ~file st model in
              st, Some Sat
          end
      in
      let st =
        State.set check_state
          { t with answers = Response_loaded answers; } st
      in
      answers st

  let eval_term st ~file ~loc term =
    let value = eval st ~file ~loc term in
    if State.get State.debug st then
      Format.eprintf "[model][eval][%a] @[<hov>%a -> %a@]@\n@."
        Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
        Dolmen.Std.Expr.Term.print term Value.print value;
    Value.extract_exn ~ops:Bool.ops value

  let eval_def { file; loc; contents = defs; } st =
    List.fold_left (fun st (cst, func) ->
        let value = eval st ~file ~loc func in
        define_value st cst value
      ) st defs

  let eval_hyp st { file; loc; contents = hyp; } =
    let res = eval_term st ~file ~loc hyp in
    if res then st else
      State.error ~file ~loc st bad_model `Hyp

  let eval_goal st { file; loc; contents = goal; } =
    let res = eval_term st ~file ~loc goal in
    if not res then st else
      State.error ~file ~loc st bad_model `Goal

  let eval_clause st { file; loc; contents = clause; } =
    let l = List.map (eval_term st ~file ~loc) clause in
    if List.exists (fun x -> x) l then st else
      State.error ~file ~loc st bad_model `Clause

  let check_aux st t = function
    | Unsat loc ->
      let file = State.get State.response_file st in
      State.warn ~file ~loc st cannot_check_proofs ()
    | Error loc ->
      let file = State.get State.response_file st in
      State.error ~file ~loc st error_in_response ()
    | Sat ->
      let st = List.fold_right eval_def t.defs st in
      let st = List.fold_left eval_hyp st t.hyps in
      let st = List.fold_left eval_goal st t.goals in
      let st = List.fold_left eval_clause st t.clauses in
      st

  let check st (c : Typer_Pipe.typechecked Typer_Pipe.stmt) =
    let st =
      if State.get check_model st then
        let t = State.get check_state st in
        let file = State.get State.logic_file st in
        let loc = Dolmen.Std.Loc.{ file = file.loc; loc = c.loc; } in
        match c.contents with
        | #Typer_Pipe.exit
        | #Typer_Pipe.decls
        | #Typer_Pipe.get_info
        | #Typer_Pipe.set_info -> st
        | #Typer_Pipe.stack_control ->
          State.error ~file ~loc st assertion_stack_not_supported ()
        | `Defs defs ->
          let new_defs = pack_abstract_defs ~file ~loc defs in
          State.set check_state { t with defs = new_defs :: t.defs; } st
        | `Hyp contents ->
          let assertion = { file; loc; contents; } in
          State.set check_state { t with hyps = assertion :: t.hyps; } st
        | `Goal contents ->
          let assertion = { file; loc; contents; } in
          State.set check_state { t with goals = assertion :: t.goals; } st
        | `Clause contents ->
          let assertion = { file; loc; contents; } in
          State.set check_state { t with clauses = assertion :: t.clauses; } st
        | `Solve l ->
          begin match get_answer st with
            | _, None ->
              State.error ~file ~loc st missing_answer ()
            | st, Some answer ->
              let local_hyps = List.map (fun contents -> { file; loc; contents; }) l in
              let t = { t with hyps = local_hyps @ t.hyps; } in
              let st = check_aux st t answer in
              State.set check_state { t with hyps = []; goals = []; clauses = []; } st
          end
      else
        st
    in
    st, c

end
