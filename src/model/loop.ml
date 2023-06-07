
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Model(/Proof) check *)

(* State *)
(* ************************************************************************ *)

type typed_model = Model.t
type parsed_model = Dolmen.Std.Statement.defs list
type cst = Dolmen.Std.Expr.term_cst
type term = Dolmen.Std.Expr.Term.t

type 't located = {
  contents : 't;
  loc : Dolmen.Std.Loc.full;
  file : Dolmen_loop.Logic.language Dolmen_loop.State.file;
}

type acc =
  | Def of (cst * term) list located
  | Hyp of term located
  | Goal of term located
  | Clause of term list located

type answer =
  | None
  | Unsat of Dolmen.Std.Loc.full
  | Error of Dolmen.Std.Loc.full
  | Sat of {
      parsed : parsed_model;
      model : typed_model;
      delayed : acc list Model.C.t;
    }

type 'st input =
  | Init
  | Response_loaded of ('st -> 'st * answer)

type 'st t = {
  answer : answer;
  input : 'st input;
}


(* Helper functions *)
(* ************************************************************************ *)

let empty = {
  input = Init;
  answer = None;
}

let file_loc_of_acc = function
  | Def { file; loc; _ }
  | Hyp { file; loc; _ }
  | Goal { file; loc; _ }
  | Clause { file; loc; _ } -> file, loc

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

let pp_defs_loc ~file fmt (defs : Dolmen.Std.Statement.defs) =
  let locs = List.map (fun (def : Dolmen.Std.Statement.def) ->
      Dolmen.Std.Loc.loc file def.loc) defs.contents in
  Format.pp_print_list
    ~pp_sep:Format.pp_print_space Dolmen.Std.Loc.fmt fmt locs

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

let parsed_model =
  Dolmen_loop.Report.Warning.mk ~code ~mnemonic:"parsed-model"
    ~message:(fun fmt (file, l) ->
        Format.fprintf fmt
          "@[<v>@[<hov>%a :@]@ %a@]"
          Format.pp_print_text
          "The model definitions at the following locations \
           have been parsed but not typed"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (pp_defs_loc ~file)) l)
    ~hints:[fun _ ->
            Some (Format.dprintf
                    "This is an internal error, plean report upstream, ^^")]
    ~name:"Parsed model" ()

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
        Format.fprintf fmt "%a" Format.pp_print_text
          "This statement requires a model in the response file in \
           order to be evaluated, but no model was found.")
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
      and type env = Dolmen_loop.Typer.T.env)
    (Typer_Pipe : Dolmen_loop.Typer.S
     with type state := State.t
      and type env = Typer.env
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
      st =
    st
    |> State.set check_model check_model_value
    |> State.set check_state empty


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

  let eval ~reraise_for_delayed_eval ~file ~loc st model term =
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
    | Model.Incorrect_extension (cst, args, ret) ->
      _err bad_extension (cst, args, ret)
    | Fp.Unhandled_exponand_and_mantissa { ew; mw } ->
      _err unhandled_float_exponand_and_mantissa (ew, mw)
    | Eval.Undefined_constant c as exn ->
      if reraise_for_delayed_eval
      then raise exn
      else _err undefined_constant c
    | Model.Partial_interpretation (cst, args) as exn ->
      if reraise_for_delayed_eval
      then raise exn
      else _err partial_interpretation (cst, args)

  (* Typing models *)
  (* ************************************************************************ *)

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
          let value = eval ~reraise_for_delayed_eval:false ~file ~loc st model func in
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

  let are_defs_declared st (defs : Dolmen.Std.Statement.defs) =
    let input = `Response (State.get State.response_file st) in
    Typer.typing_wrap ~input st ~f:(fun env ->
        List.for_all (fun (def : Dolmen.Std.Statement.def) ->
            match Dolmen_loop.Typer.T.find_bound env def.id with
            | #Dolmen_loop.Typer.T.not_found ->
              false
            | _ -> true
          ) defs.contents
      )

  let type_model_aux ~input ?attrs st model parsed_defs =
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
      let st, defs_declared = are_defs_declared st defs in
      if defs_declared then
        let st, model = type_model_aux ~input ?attrs st model defs in
        type_model_defined ~input ?attrs st model r
      else
        st, model, l

  let type_model_partial ?attrs st parsed =
    let file = State.get State.response_file st in
    let input = `Response file in
    let st = Typer.push ~input st 1 in
    let st, model, parsed = type_model_defined ~input ?attrs st Model.empty parsed in
    let st = Typer.pop st ~input 1 in
    st, model, parsed


  (* Pipe function *)
  (* ************************************************************************ *)

  let next_answer st =
    let t = State.get check_state st in
    match t.input with
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
            | Unsat -> st, Unsat loc
            | Error _ -> st, Error loc
            | Sat None ->
              st, Sat { parsed = []; model = Model.empty; delayed = Model.C.empty; }
            | Sat Some parsed ->
              st, Sat { parsed; model = Model.empty; delayed = Model.C.empty; }
          end
      in
      let st =
        State.set check_state
          { t with input = Response_loaded answers; } st
      in
      answers st

  (* TODO: the file and loc parameters are the file and loc of the statement
     in the original problem (and not the model file) that triggered
     this module to read the model. Use multi-locs to better explain
     that. *)
  let get_answer ~file ~loc st t = function
    | (Unsat _ | Error _ | Sat _) as a -> st, a
    | None ->
      let st, answer = next_answer st in
      let st =
        match answer with
        | None ->
          State.error ~file ~loc st missing_answer ()
        | Error loc ->
          let file = State.get State.response_file st in
          State.error ~file ~loc st error_in_response ()
        | Unsat loc ->
          let file = State.get State.response_file st in
          State.warn ~file ~loc st cannot_check_proofs ()
        | Sat _ ->
          st
      in
      let st = State.set check_state { t with answer; } st in
      st, answer

  let reset st =
    let t = State.get check_state st in
    State.set check_state { t with answer = None; } st

  (* Evaluation functions *)
  (* ************************************************************************ *)

  let rec eval_loop st parsed model (delayed : acc list Model.C.t) (acc : acc) =
    let st, newly_defined, parsed = type_model_partial st parsed in
    let model = Model.disjoint_union model newly_defined in
    let st, model, delayed = eval_newly_defined st model delayed newly_defined in
    let st, model, delayed = eval_acc st model delayed acc in
    st, parsed, model, delayed

  and eval_newly_defined st model delayed newly_defined =
    let newly_defined_and_needed =
      Model.C.merge (fun _cst value_opt acc_opt ->
          match value_opt, acc_opt with
            | Some _, (Some _ as res) -> res
            | None as res, _
            | Some _, (None as res) -> res
        ) (Model.csts newly_defined) delayed
    in
    eval_delayed st model delayed newly_defined_and_needed

  and eval_delayed st model delayed to_eval =
    Model.C.fold (fun cst acc_list (st, model, delayed) ->
        let delayed = Model.C.remove cst delayed in
        List.fold_left (fun (st, model, delayed) acc ->
            eval_acc st model delayed acc
          ) (st, model, delayed) acc_list
      ) to_eval (st, model, delayed)

  and eval_acc_direct ~reraise st model delayed = function
    | Def def -> eval_def ~reraise st model delayed def
    | Hyp hyp ->
      let st = eval_hyp ~reraise st model hyp in
      st, model, delayed
    | Goal g ->
      let st = eval_goal ~reraise st model g in
      st, model, delayed
    | Clause clause ->
      let st = eval_clause ~reraise st model clause in
      st, model, delayed

  and eval_acc st model delayed (acc : acc) =
    try
      eval_acc_direct ~reraise:true st model delayed acc
    with
    | Eval.Undefined_constant c
    | Model.Partial_interpretation (c, _) ->
      if State.get State.debug st then begin
        let _file, loc = file_loc_of_acc acc in
        Format.eprintf
          "[model][eval][%a] @[<hv 2>delayed on %a@]@\n@."
          Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
          Dolmen.Std.Expr.Term.Const.print c
      end;
      let delayed = Model.C.update c (function
          | None -> Some [acc]
          | Some l -> Some (acc :: l)
        ) delayed in
      st, model, delayed

  and eval_prop ~reraise ~file ~loc st model term =
    if State.get State.debug st then
      Format.eprintf "[model][eval][%a] @[<hov>%a@]@."
        Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
        Dolmen.Std.Expr.Term.print term;
    let value = eval ~reraise_for_delayed_eval:reraise ~file ~loc st model term in
    if State.get State.debug st then
      Format.eprintf "[model][eval][%a] @[<hov>result -> %a@]@\n@."
        Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
        Value.print value;
    Value.extract_exn ~ops:Bool.ops value

  and eval_def ~reraise st model delayed { file; loc; contents = defs; } =
    let newly_defined =
      List.fold_left (fun newly_defined (cst, func) ->
          if State.get State.debug st then begin
            Format.eprintf "[model][eval][%a] @[<hv 2>%a ->@ @[<hov>%a@]@]@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Dolmen.Std.Expr.Term.print func
          end;
          let value = eval ~reraise_for_delayed_eval:reraise ~file ~loc st model func in
          if State.get State.debug st then begin
            Format.eprintf "[model][eval][%a] @[<hv 2>%a ->@ @[<hov>%a@]@]@\n@."
              Dolmen.Std.Loc.fmt_compact (Dolmen.Std.Loc.full_loc loc)
              Dolmen.Std.Expr.Term.Const.print cst
              Value.print value
          end;
        Model.Cst.add cst value newly_defined
        ) Model.empty defs
    in
    let model = Model.disjoint_union model newly_defined in
    let st, model, delayed = eval_newly_defined st model delayed newly_defined in
    (st, model, delayed)

  and eval_hyp ~reraise st model { file; loc; contents = hyp; } =
    let res = eval_prop ~reraise ~file ~loc st model hyp in
    if res then st else
      State.error ~file ~loc st bad_model `Hyp

  and eval_goal ~reraise st model { file; loc; contents = goal; } =
    let res = eval_prop ~reraise ~file ~loc st model goal in
    if not res then st else
      State.error ~file ~loc st bad_model `Goal

  and eval_clause ~reraise st model { file; loc; contents = clause; } =
    let l = List.map (eval_prop ~reraise ~file ~loc st model) clause in
    if List.exists (fun x -> x) l then st else
      State.error ~file ~loc st bad_model `Clause

  (* Check functions functions *)
  (* ************************************************************************ *)

  let check_acc st acc =
    let file, loc = file_loc_of_acc acc in
    let t = State.get check_state st in
    let st, answer = get_answer ~file ~loc st t t.answer in
    match answer with
    | Sat { parsed; model; delayed; } ->
      let st, parsed, model, delayed = eval_loop st parsed model delayed acc in
      let t = { t with answer = Sat { parsed; model; delayed; } } in
      State.set check_state t st
    | _ -> st


  let check_defs st ~file ~loc defs =
    let new_defs = pack_abstract_defs ~file ~loc defs in
    check_acc st (Def new_defs)

  let check_hyps st ~file ~loc contents =
    let hyp = { file; loc; contents; } in
    check_acc st (Hyp hyp)

  let check_goal st ~file ~loc contents =
    let g = { file; loc; contents; } in
    check_acc st (Goal g)

  let check_clause st ~file ~loc contents =
    let clause = { file; loc; contents; } in
    check_acc st (Clause clause)

  let check_solve st ~(file : _ Dolmen_loop.State.file) ~loc l =
    let local_hyps = List.map (fun contents -> { file; loc; contents; }) l in
    let st =
      List.fold_left (fun st local_hyp ->
          check_acc st (Hyp local_hyp)
        ) st local_hyps
    in
    let t = State.get check_state st in
    let st =
      match t.answer with
      | Sat { parsed; model; delayed; } ->
        (* Warn about parsed and untyped model definitions *)
        let st =
          match parsed with
          | [] -> st
          | _ :: _ -> State.warn st parsed_model (file.loc, parsed)
        in
        (* Error out if there are any un-evaluated statements *)
        if Model.C.is_empty delayed then st
        else begin
          let accs = Model.C.bindings delayed in
          let accs = List.concat_map (fun (c, l) ->
              List.map (fun acc -> (c, acc)) l) accs in
          let accs = List.sort (fun (_, acc) (_, acc') ->
              let _, loc = file_loc_of_acc acc in
              let _, loc' = file_loc_of_acc acc' in
              Dolmen.Std.Loc.compare loc.loc loc'.loc
            ) accs in
          match accs with
          | [] -> assert false
          | (_, acc) :: _ ->
            (* this call should raise an exception/error *)
            let _ = eval_acc_direct ~reraise:false st model delayed acc in
            assert false
        end
      | _ -> st
    in
    reset st

  (* Pipe/toplevel function *)
  (* ************************************************************************ *)

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
