
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Model(/Proof) check *)

(* State *)
(* ************************************************************************ *)

type typed_model = Model.t
type parsed_model = Dolmen.Std.Statement.defs list
type cst = Dolmen.Std.Expr.term_cst
type term = Dolmen.Std.Expr.Term.t

type mode =
  | Full
  | Interleave

type 'model answer =
  | Sat of 'model
  | Unsat of Dolmen.Std.Loc.full
  | Error of Dolmen.Std.Loc.full

type 'st answers =
  | Init
  | Response_loaded of ('st -> 'st * parsed_model answer option)

type 't located = {
  contents : 't;
  loc : Dolmen.Std.Loc.full;
  file : Dolmen_loop.Logic.language Dolmen_loop.State.file;
}

type acc = {
  defs : (cst * term) list located list;
  hyps : term located list;
  goals : term located list;
  clauses : term list located list;
}

type internal =
  | Full_check of { acc : acc; }
  | Interleaved of { answer : (typed_model * parsed_model) answer option; }

type 'st t = {
  internal : internal;
  answers : 'st answers;
}

let empty_acc = { defs = []; hyps = []; goals = []; clauses = []; }

let empty ~mode () = {
  answers = Init;
  internal =
    match mode with
    | Full -> Full_check { acc = empty_acc; }
    | Interleave -> Interleaved { answer = None; }
  ;
}


(* Warnings and errors *)
(* ************************************************************************ *)

module E = Dolmen.Std.Expr

let pp_wrap pp fmt x =
  Format.fprintf fmt "`%a`" pp x

let pp_app fmt (cst, args) =
  match (E.Term.Const.get_tag cst E.Tags.pos) with
  | None | Some Dolmen.Std.Pretty.Prefix ->
    Format.fprintf fmt "(%a@ %a)"
      E.Term.Const.print cst
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Value.print) args
  | Some Dolmen.Std.Pretty.Infix ->
    let pp_sep fmt () = Format.fprintf fmt " %a@ " E.Term.Const.print cst in
    Format.pp_print_list ~pp_sep Value.print fmt args

let code =
  Dolmen_loop.Code.create
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
          "The following variable is not defined/let-bound, and thus \
          has no value: %a"
          (pp_wrap Dolmen.Std.Expr.Term.Var.print) v)
      ~name:"Undefined variable in Model verification" ()

let undefined_constant =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"undefined-constant"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The following constant is not defined, and thus \
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

let partial_interpretation =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"partial-dstr"
    ~message:(fun fmt (cst, args) ->
        Format.fprintf fmt
          "The symbol %a is only partially interpreted/defined,@ \
           and the following application does not have an intepretation:@ \
           @[<hov 2>%a@]"
          (pp_wrap Dolmen.Std.Expr.Term.Const.print) cst
          pp_app (cst, args))
    ~name:"Partial Destructor" ()

(* TODO: add loc for the definition of the symbol being incorrectly extended *)
let bad_extension =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"bad-extension"
    ~message:(fun fmt (cst, args, ret) ->
        Format.fprintf fmt
          "The extension for symbol %a returned a non-conforming value:@ \
           @[<hov 2>%a@ -> %a@]"
          (pp_wrap Dolmen.Std.Expr.Term.Const.print) cst
          pp_app (cst, args) Value.print ret
      )
    ~name:"Bad extension" ()

let unhandled_float_exponand_and_mantissa =
  Dolmen_loop.Report.Error.mk ~code ~mnemonic:"unhandled-float-sizes"
    ~message:(fun fmt (ew, mw) ->
        Format.fprintf fmt
          "The following size for exponand and mantissa are not currently
          handled by dolmen: (%d, %d)." ew mw)
    ~hints:[(fun _ -> Some (Format.dprintf "%a"
        Format.pp_print_text
          "This is a current implementation limitation of dolmen. \
           Please report upstream if encounter this error, ^^")); ]
    ~name:"Unhandled Floating point sizes" ()

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
      ~check_model_mode
      st =
    st
    |> State.set check_model check_model_value
    |> State.set check_state (empty ~mode:check_model_mode ())


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

  let eval ~file ~loc st model term =
    let _err err args =
      raise (State.Error (State.error st ~file ~loc err args))
    in
    let env = Env.mk model ~builtins in
    try
      Eval.eval env term
    with
    | Eval.Quantifier -> _err fo_model ()
    | Eval.Unhandled_builtin b -> _err unhandled_builtin b
    | Eval.Undefined_variable v -> _err undefined_variable v
    | Eval.Undefined_constant c -> _err undefined_constant c
    | Model.Partial_interpretation (cst, args) ->
      _err partial_interpretation (cst, args)
    | Model.Incorrect_extension (cst, args, ret) ->
      _err bad_extension (cst, args, ret)
    | Fp.Unhandled_exponand_and_mantissa { ew; mw } ->
      _err unhandled_float_exponand_and_mantissa (ew, mw)

  (* Typing models *)
  (* ************************************************************************ *)

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
          | `Instanceof _ -> None (* TODO: warning/error ? *)
          | `Term_def (_id, cst, ty_params, term_params, body) ->
            let func = Dolmen.Std.Expr.Term.lam (ty_params, term_params) body in
            Some (cst, func)
        ) typed_defs
    in
    { contents; loc; file; }

  let record_defs st model (parsed_defs : Dolmen.Std.Statement.defs) typed_defs =
    let file = State.get State.response_file st in
    List.fold_left2 (fun (st, model) (parsed : Dolmen.Std.Statement.def) def ->
        let loc = Dolmen.Std.Loc.{ file = file.loc; loc = parsed.loc; } in
        match def with
        | `Type_def _ ->
          (State.error ~file ~loc st type_def_in_model (), model)
        | `Term_def (_id, cst, ty_params, term_params, body) ->
          let func = Dolmen.Std.Expr.Term.lam (ty_params, term_params) body in
          if State.get State.debug st then
            Format.eprintf "[model][typed][%a] @[<hov>%a := %a@]@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Dolmen.Std.Expr.Term.print func;
          let value = eval ~file ~loc st model func in
          if State.get State.debug st then
            Format.eprintf "[model][value][%a] @[<hov>%a -> %a@]@\n@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Value.print value;
          let model = Model.Cst.add cst value model in
          (st, model)
        | `Instanceof (_id, cst, ty_args, ty_params, term_params, body) ->
          assert (ty_params = []);
          let pp_sep fmt () = Format.fprintf fmt ", @ " in
          if State.get State.debug st then
            Format.eprintf "[model][typed][%a] @[<hov>%a(%a) := %a@]@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Ty.print) ty_args
              Dolmen.Std.Expr.Term.print (Dolmen.Std.Expr.Term.lam ([], term_params) body);
          let model = Fun.add_ad_hoc_instance model ~cst ~ty_args ~term_params ~body in
          if State.get State.debug st then
            Format.eprintf "[model][typed] %a@." Model.print model;
          (st, model)
      ) (st, model) parsed_defs.contents typed_defs

  let is_id_declared (ty_state : Dolmen_loop.Typer.T.state) id =
    match Dolmen_loop.Typer.T.find_global_st ty_state id with
    | #Dolmen_loop.Typer.T.not_found -> false
    | #Dolmen_loop.Typer.T.cst -> true

  let are_defs_declared st (defs : Dolmen.Std.Statement.defs) =
    let typer_state = State.get Typer.ty_state st in
    let ty_state = Dolmen_loop.Typer.typer_state typer_state in
    List.for_all (fun (def : Dolmen.Std.Statement.def) ->
        is_id_declared ty_state def.id
      ) defs.contents

  let type_model_aux ~input ?attrs (st, model) parsed_defs =
    if State.get State.debug st then
      Format.eprintf "[model][parsed] @[<hov>%a@]@."
        Dolmen.Std.Statement.(print_group print_def) parsed_defs;
    let st, defs =
      Typer.defs ~mode:`Use_declared_id st ~input ?attrs parsed_defs
    in
    (* Record inferred abstract values *)
    let model =
      List.fold_left (fun model c ->
          let value = Value.abstract_cst c in
          Model.Cst.add c value model
        ) model (Typer.pop_inferred_model_constants st)
    in
    (* Record the explicit definitions *)
    let st, model = record_defs st model parsed_defs defs in
    st, model

  let rec type_model_defined ~input ?attrs st model = function
    | [] -> st, model, []
    | (defs :: r) as l ->
      if are_defs_declared st defs then
        let st, model = type_model_aux ~input ?attrs (st, model) defs in
        type_model_defined ~input ?attrs st model r
      else
        st, model, l

  let type_model_full ?attrs st l =
    let file = State.get State.response_file st in
    let input = `Response file in
    let st = Typer.push ~input st 1 in
    let st, model =
      List.fold_left (type_model_aux ~input ?attrs) (st, Model.empty) l
    in
    let st = Typer.pop st ~input 1 in
    st, model

  let type_model_partial ?attrs st model parsed =
    let file = State.get State.response_file st in
    let input = `Response file in
    let st = Typer.push ~input st 1 in
    let st, model, parsed = type_model_defined ~input ?attrs st model parsed in
    let st = Typer.pop st ~input 1 in
    st, model, parsed



  (* Pipe function *)
  (* ************************************************************************ *)

  let next_answer st =
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
            | Sat None -> st, Some (Sat [])
            | Sat Some model -> st, Some (Sat model)
          end
      in
      let st =
        State.set check_state
          { t with answers = Response_loaded answers; } st
      in
      answers st

  (* TODO: the file and loc parameters are the file and loc of the statement
     in the original problem (and not the model file) that triggered
     this module to read the model. Use multi-locs to better explain
     that. *)
  let fast_check_answer ~file ~loc st t = function
    | Some _ as a -> st, a
    | None ->
      let st, answer = next_answer st in
      let st, answer =
        match answer with
        | None ->
          State.error ~file ~loc st missing_answer (), None
        | Some Error loc as a ->
          let file = State.get State.response_file st in
          State.error ~file ~loc st error_in_response (), a
        | Some Unsat loc as a->
          let file = State.get State.response_file st in
          State.warn ~file ~loc st cannot_check_proofs (), a
        | Some Sat parsed_model ->
          st, Some (Sat (Model.empty, parsed_model))
      in
      let t = { t with internal = Interleaved { answer; } } in
      let st = State.set check_state t st in
      st, answer

  let by_mode ~file ~loc st ~interleaved ~full =
    let t = State.get check_state st in
    match t.internal with
    | Interleaved { answer } ->
      let st, answer = fast_check_answer ~file ~loc st t answer in
      begin match answer with
        | Some Sat (model, parsed) ->
          let st, model, parsed = type_model_partial st model parsed in
          let st, model = interleaved st parsed model in
          let t = { t with internal = Interleaved { answer = Some (Sat (model, parsed)); } } in
          State.set check_state t st
        | _ -> st
      end
    | Full_check { acc } ->
      let st, acc = full st acc in
      let t = { t with internal = Full_check { acc }; } in
      State.set check_state t st

  let reset st =
    let t = State.get check_state st in
    let internal =
      match t.internal with
      | Interleaved _ -> Interleaved { answer = None; }
      | Full_check _ -> Full_check { acc = empty_acc; }
    in
    State.set check_state { t with internal } st

  let eval_term ~file ~loc st model term =
    if State.get State.debug st then
      Format.eprintf "[model][eval][%a] @[<hov>%a@]@."
        Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
        Dolmen.Std.Expr.Term.print term;
    let value = eval ~file ~loc st model term in
    if State.get State.debug st then
      Format.eprintf "[model][eval][%a] @[<hov>result -> %a@]@\n@."
        Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
        Value.print value;
    Value.extract_exn ~ops:Bool.ops value

  let eval_def { file; loc; contents = defs; } (st, model) =
    let model =
      List.fold_left (fun model (cst, func) ->
        let value = eval ~file ~loc st model func in
        Model.Cst.add cst value model
        ) model defs
    in
    (st, model)

  let eval_hyp model st { file; loc; contents = hyp; } =
    let res = eval_term ~file ~loc st model hyp in
    if res then st else
      State.error ~file ~loc st bad_model `Hyp

  let eval_goal model st { file; loc; contents = goal; } =
    let res = eval_term ~file ~loc st model goal in
    if not res then st else
      State.error ~file ~loc st bad_model `Goal

  let eval_clause model st { file; loc; contents = clause; } =
    let l = List.map (eval_term ~file ~loc st model) clause in
    if List.exists (fun x -> x) l then st else
      State.error ~file ~loc st bad_model `Clause

  let check_defs st ~file ~loc defs =
    let new_defs = pack_abstract_defs ~file ~loc defs in
    by_mode ~file ~loc st
      ~interleaved:(fun st _parsed model ->
          let st, model = eval_def new_defs (st, model) in
          st, model
        )
      ~full:(fun st acc ->
          let acc = { acc with defs =  new_defs :: acc.defs; } in
          st, acc
        )

  let check_hyps st ~file ~loc contents =
    let assertion = { file; loc; contents; } in
    by_mode ~file ~loc st
      ~interleaved:(fun st _parsed model ->
          let st = eval_hyp model st assertion in
          st, model
        )
      ~full:(fun st acc ->
          let acc = { acc with hyps = assertion :: acc.hyps; } in
          st, acc
        )

  let check_goal st ~file ~loc contents =
    let assertion = { file; loc; contents; } in
    by_mode ~file ~loc st
      ~interleaved:(fun st _parsed model ->
          let st = eval_goal model st assertion in
          st, model
        )
      ~full:(fun st acc ->
          let acc = { acc with goals = assertion :: acc.goals; } in
          st, acc
         )

  let check_clause st ~file ~loc contents =
    let assertion = { file; loc; contents; } in
    by_mode ~file ~loc st
      ~interleaved:(fun st _parsed model ->
          let st = eval_clause model st assertion in
          st, model
        )
      ~full:(fun st acc ->
          let acc = { acc with clauses = assertion :: acc.clauses; } in
          st, acc
        )

  let check_solve st ~(file : _ Dolmen_loop.State.file) ~loc l =
    let no_loc = Dolmen.Std.Loc.{ file = file.loc; loc = Dolmen.Std.Loc.no_loc; } in
    let local_hyps = List.map (fun contents -> { file; loc = no_loc; contents; }) l in
    let st =
      by_mode ~file ~loc st
        ~interleaved:(fun st _parsed model ->
            let st = List.fold_left (eval_hyp model) st local_hyps in
            st, Model.empty
          )
        ~full:(fun st acc ->
            match next_answer st with
            | _, None ->
              State.error ~file ~loc st missing_answer (), acc
            | st, Some Unsat loc ->
              let file = State.get State.response_file st in
              State.warn ~file ~loc st cannot_check_proofs (), acc
            | st, Some Error loc ->
              let file = State.get State.response_file st in
              State.error ~file ~loc st error_in_response (), acc
            | st, Some Sat model ->
              let st, model = type_model_full st model in
              let acc = { acc with hyps = local_hyps @ acc.hyps; } in
              let st, model = List.fold_right eval_def acc.defs (st, model) in
              let st = List.fold_left (eval_hyp model) st acc.hyps in
              let st = List.fold_left (eval_goal model) st acc.goals in
              let st = List.fold_left (eval_clause model) st acc.clauses in
              let acc = { defs = []; hyps = []; goals = []; clauses = []; } in
              st, acc
          )
    in
    reset st

  let check st (c : Typer_Pipe.typechecked Typer_Pipe.stmt) =
    let st =
      if State.get check_model st then
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
            check_defs ~file ~loc st defs
          | `Hyp contents ->
            check_hyps ~file ~loc st contents
          | `Goal contents ->
            check_goal ~file ~loc st contents
          | `Clause contents ->
            check_clause ~file ~loc st contents
          | `Solve l ->
            check_solve ~file ~loc st l
      else
        st
    in
    st, c

end
