
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = sig

  type solve_st

  module T : Dolmen_type.Tff.S
    with type 'a Tag.t = 'a Dolmen.Tag.t
     and type Ty.t = Dolmen.Expr.ty
     and type Ty.Var.t = Dolmen.Expr.ty_var
     and type Ty.Const.t = Dolmen.Expr.ty_const
     and type T.t = Dolmen.Expr.term
     and type T.Var.t = Dolmen.Expr.term_var
     and type T.Const.t = Dolmen.Expr.term_const
     and type T.Cstr.t = Dolmen.Expr.term_const

  include Typer_intf.S
    with type state := (Parser.language, T.state, solve_st) Dolmen.State.state
     and type ty := Dolmen.Expr.ty
     and type ty_var := Dolmen.Expr.ty_var
     and type ty_const := Dolmen.Expr.ty_const
     and type term := Dolmen.Expr.term
     and type term_var := Dolmen.Expr.term_var
     and type term_const := Dolmen.Expr.term_const
     and type formula := Dolmen.Expr.formula

  val report_error : Format.formatter -> T.err -> unit

end

module Make(S : State_intf.Typer) = struct

  (* Shadowing *)
  (* ************************************************************************ *)

  let print_reason fmt r =
    match (r : Dolmen_type.Tff.reason) with
    | Inferred loc ->
      Format.fprintf fmt "inferred at %a" Dolmen.ParseLocation.fmt loc
    | Declared loc ->
      Format.fprintf fmt "declared at %a" Dolmen.ParseLocation.fmt loc

  let print_shadowing_reasons fmt (id, old, cur) =
    Format.fprintf fmt "Shadowing:@ %a was %a@ and is now %a"
      Dolmen.Id.print id
      print_reason old
      print_reason cur

  (* Warnings *)
  (* ************************************************************************ *)

  let add_warning, get_warnings =
    let l = ref [] in
    let add loc msg = l := (loc, msg) :: !l in
    let get () = let res = !l in l := []; res in
    add, get

  let fmt_warning loc format =
    Format.kasprintf (fun msg -> add_warning loc msg) format

  module Warn = struct

    let reason_loc r =
    match (r : Dolmen_type.Tff.reason) with
      | Inferred loc
      | Declared loc -> loc

    let binding_reason = function
      | `Not_found -> assert false
      | `Ty (_, reason)
      | `Cstr (_, reason)
      | `Term (_, reason)
      | `Field (_, reason) -> reason

    let shadow id old cur =
      let old_reason = binding_reason old in
      let new_reason = binding_reason cur in
      let loc = reason_loc new_reason in
      fmt_warning loc "%a"
          print_shadowing_reasons (id, old_reason, new_reason)

    let unused_ty_var loc v =
      fmt_warning loc
          "Quantified type variable `%a` is unused"
          Dolmen.Expr.Print.ty_var v

    let unused_term_var loc v =
      fmt_warning loc
        "Quantified term variable `%a` is unused"
        Dolmen.Expr.Print.term_var v

    let error_in_attribute loc exn =
      fmt_warning loc
        "Error while type-checking an attribute:@ %s"
        (Printexc.to_string exn)

    let not_found _ _ = ()

    let superfluous_destructor _ _ _ _ = ()

  end

  (* Dolmen_type functors instantiation *)
  (* ************************************************************************ *)

  module T = Dolmen_type.Tff.Make
      (Dolmen.Tag)(Dolmen.Expr.Ty)(Dolmen.Expr.Term)(Warn)

  (* Definitions builtin *)
  module Def = Dolmen_type.Def.Declare(T)

  (* Tptp builtins *)
  module Tptp_Base =
    Dolmen_type.Base.Tptp.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)
  module Tptp_Arith =
    Dolmen_type.Arith.Tptp.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)

  (* Stmlib theories *)
  module Smtlib2_Base =
    Dolmen_type.Base.Smtlib2.Tff(T)(Dolmen.Expr.Tags)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)
  module Smtlib2_Ints =
    Dolmen_type.Arith.Smtlib2.Int.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term.Int)
  module Smtlib2_Reals =
    Dolmen_type.Arith.Smtlib2.Real.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term.Real)
  module Smtlib2_Reals_Ints =
    Dolmen_type.Arith.Smtlib2.Real_Int.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)
  module Smtlib2_Arrays =
    Dolmen_type.Arrays.Smtlib2.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)
  module Smtlib2_Bitv =
    Dolmen_type.Bitv.Smtlib2.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)

  (* Zf *)
  module Zf_Base =
    Dolmen_type.Base.Zf.Tff(T)(Dolmen.Expr.Tags)


  (* Report type errors *)
  (* ************************************************************************ *)

  let print_res fmt res =
    match (res : T.res) with
    | T.Ttype -> Format.fprintf fmt "Type"
    | T.Ty ty -> Format.fprintf fmt "the type@ %a" Dolmen.Expr.Ty.print ty
    | T.Term t -> Format.fprintf fmt "the term@ %a" Dolmen.Expr.Term.print t
    | T.Tags _ -> Format.fprintf fmt "some tags"

  let print_opt pp fmt = function
    | None -> Format.fprintf fmt "<none>"
    | Some x -> pp fmt x

  let report_error fmt = function
    (* Core Typechecking Errors *)
    | T.Infer_type_variable ->
      Format.fprintf fmt "Cannot infer the type of a variable"
    | T.Expected (expect, got) ->
      Format.fprintf fmt "Expected %s but got %a" expect (print_opt print_res) got
    | T.Bad_op_arity (s, i, j) ->
      Format.fprintf fmt "Bad arity for builtin '%s':@ expected %d arguments but got %d" s i j
    | T.Bad_ty_arity (c, i) ->
      Format.fprintf fmt "Bad arity (expected %d arguments) for type constant@ %a"
        i Dolmen.Expr.Print.ty_const c
    | T.Bad_cstr_arity (c, i, j) ->
      Format.fprintf fmt
        "Bad arity (expected %d type argument, and %d term arguments) for term constructor@ %a"
        i j Dolmen.Expr.Print.term_const c
    | T.Bad_term_arity (c, i, j) ->
      Format.fprintf fmt
        "Bad arity (expected %d type argument, and %d term arguments) for term constant@ %a"
        i j Dolmen.Expr.Print.term_const c
    | T.Var_application v ->
      Format.fprintf fmt "Cannot apply arguments to term variable@ %a" Dolmen.Expr.Print.id v
    | T.Ty_var_application v ->
      Format.fprintf fmt "Cannot apply arguments to type variable@ %a" Dolmen.Expr.Print.id v
    | T.Type_mismatch (t, expected) ->
      Format.fprintf fmt "The term:@ %a@ has type@ %a@ but was expected to be of type@ %a"
        Dolmen.Expr.Term.print t
        Dolmen.Expr.Ty.print (Dolmen.Expr.Term.ty t)
        Dolmen.Expr.Ty.print expected
    | T.Quantified_var_inference ->
      Format.fprintf fmt "Cannot infer type for a quantified variable"
    | T.Unhandled_builtin b ->
      Format.fprintf fmt
        "The following Dolmen builtin is currently not handled@ %a.@ Please report upstream"
        Dolmen.Term.print_builtin b
    | T.Cannot_tag_tag ->
      Format.fprintf fmt "Cannot apply a tag to another tag (only expressions)"
    | T.Cannot_tag_ttype ->
      Format.fprintf fmt "Cannot apply a tag to the Ttype constant"
    | T.Cannot_find id ->
      Format.fprintf fmt "Unbound identifier:@ '%a'" Dolmen.Id.print id
    | T.Type_var_in_type_constructor ->
      Format.fprintf fmt "Type variables cannot appear in the signature of a type constant"
    | T.Missing_destructor id ->
      Format.fprintf fmt
        "The destructor '%a'@ was not provided by the user implementation.@ Please report upstream."
        Dolmen.Id.print id
    | T.Higher_order_application ->
      Format.fprintf fmt "Higher-order applications are not handled by the Tff typechecker"
    | T.Higher_order_type ->
      Format.fprintf fmt "Higher-order types are not handled by the Tff typechecker"
    | T.Unbound_variables (tys, [], _) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "The following variables are not bound:@ %a"
        (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) tys
    | T.Unbound_variables ([], ts, _) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "The following variables are not bound:@ %a"
        (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) ts
    | T.Unbound_variables (tys, ts, _) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "The following variables are not bound:@ %a,@ %a"
        (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) tys
        (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) ts
    | T.Unhandled_ast ->
      Format.fprintf fmt "The typechecker did not know what to do with the term.@ Please report upstream."

    (* Tptp Arithmetic errors *)
    | Tptp_Arith.Expected_arith_type ty ->
      Format.fprintf fmt "Arithmetic type expected but got@ %a.@ %s"
        Dolmen.Expr.Ty.print ty
        "Tptp arithmetic symbols are only polymoprhic over the arithmetic types $int, $rat and $real."
    | Tptp_Arith.Cannot_apply_to ty ->
      Format.fprintf fmt "Cannot apply the arithmetic operation to type@ %a"
        Dolmen.Expr.Ty.print ty

    (* Smtlib Arithmetic errors *)
    | Smtlib2_Reals_Ints.Expected_arith_type ty ->
      Format.fprintf fmt "Arithmetic type expected but got@ %a.@ %s"
        Dolmen.Expr.Ty.print ty
        "The stmlib Reals_Ints theory requires an arithmetic type in order to correctly desugar the expression."

    (* Smtlib Bitvector errors *)
    | Smtlib2_Bitv.Invalid_bin_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a binary bitvector litteral" c
    | Smtlib2_Bitv.Invalid_hex_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a hexadecimal bitvector litteral" c

    (* Linear arithmetic *)
    | T.Uncaught_exn (Dolmen.Expr.Filter_failed_term ("linear", _t)) ->
      Format.fprintf fmt "Non-linear term."

    (* Expression filters *)
    | T.Uncaught_exn (Dolmen.Expr.Filter_failed_ty (name, _ty)) ->
      Format.fprintf fmt "Filter '%s' failed for the given type." name
    | T.Uncaught_exn (Dolmen.Expr.Filter_failed_term (name, _t)) ->
      Format.fprintf fmt "Filter '%s' failed for the given term." name

    (* Uncaught exception during type-checking *)
    | T.Uncaught_exn exn ->
      Format.fprintf fmt "Uncaught exception: %s" (Printexc.to_string exn)

    (* Catch-all *)
    | _ ->
      Format.fprintf fmt "Unknown typing error,@ please report upstream, ^^"

  let () =
    Printexc.register_printer (function
        | T.Typing_error (err, _, _) ->
          Some (Format.asprintf "@[<hov>%a@]@." report_error err)
        | _ -> None
      )


  (* Generate typing env from state *)
  (* ************************************************************************ *)

  let default_smtlib_logic = {
    Dolmen_type.Base.theories =
      [ `Core; `Reals_Ints; `Arrays; `Bitvectors; ];
    restrictions = [];
  }

  let smtlib_logic loc st =
    match st.Dolmen.State.type_smtlib_logic with
    | Some s ->
      begin match Dolmen_type.Base.smtlib_logic s with
        | Some logic -> logic
        | None ->
          (* Unknown logic, default to a reasonable combination *)
          add_warning loc (Format.asprintf "Unknown logic %s" s);
          default_smtlib_logic
      end
    | None ->
      (* Missing logic, this is a hard error in an smtlib file *)
      S.missing_smtlib_logic ()

  let builtins_of_smtlib_logic v l =
    List.fold_left (fun acc th ->
        match (th : Dolmen_type.Base.smtlib_theory) with
        | `Core -> Smtlib2_Base.parse v :: acc
        | `Ints -> Smtlib2_Ints.parse v :: acc
        | `Arrays -> Smtlib2_Arrays.parse v :: acc
        | `Bitvectors -> Smtlib2_Bitv.parse v :: acc
        | `Reals -> Smtlib2_Reals.parse v :: acc
        | `Reals_Ints -> Smtlib2_Reals_Ints.parse v :: acc
      ) [] l.Dolmen_type.Base.theories

  let restrictions_of_smtlib_logic _v l =
    Dolmen.Expr.Filter.reset ();
    List.iter (function
        | `Linear_arithmetic ->
          Dolmen.Expr.Filter.Linear.active := true
        | `Quantifier_free
        | `Difference_logic
        | `No_free_symbol -> (* TODO *) ()
      ) l.Dolmen_type.Base.restrictions

  let typing_env
      ?(loc=Dolmen.ParseLocation.mk "" 0 0 0 0)
      (st : (Parser.language, _, _) Dolmen.State.state) =
    (* Expected type for type inference (top-down) *)
    let expect =
      match st.input_lang with
      | Some Dimacs
      | Some ICNF
      | Some Tptp _ ->
        T.Typed Dolmen.Expr.Ty.prop
      | _ ->
        T.Nothing
    in
    (* Base type used for type inference (bottom) *)
    let infer_base =
      match st.input_lang with
      | Some Dimacs
      | Some ICNF -> Some Dolmen.Expr.Ty.prop
      | Some Tptp _ -> Some Dolmen.Expr.Ty.base
      | _ -> None
    in
    (* Builtins for each language *)
    let lang_builtins =
      match st.input_lang with
      | None -> assert false
      | Some Dimacs
      | Some ICNF -> []
      | Some Alt_ergo -> []
      | Some Tptp v -> [
          Tptp_Base.parse v;
          Tptp_Arith.parse v;
        ]
      | Some Zf -> [
          Zf_Base.parse;
        ]
      | Some Smtlib2 v ->
        let logic = smtlib_logic loc st in
        restrictions_of_smtlib_logic v logic;
        builtins_of_smtlib_logic v logic
    in
    let builtins = Dolmen_type.Base.merge (Def.parse :: lang_builtins) in
    T.empty_env ~st:st.type_state ~expect ?infer_base builtins


  (* Wrappers around the Type-checking module *)
  (* ************************************************************************ *)

  let typecheck (st : _ Dolmen.State.state) = st.type_check

  let def st ?attr id (t : Dolmen.Term.t) =
    let env = typing_env ?loc:t.loc st in
    begin match T.new_def ?attr env t id with
      | `Type_def (id, _, vars, body) ->
        let _ = Def.define_ty id vars body in
        st, `Type_def (id, vars, body), get_warnings ()
      | `Term_def (id, _, vars, args, body) ->
        let _ = Def.define_term id vars args body in
        st, `Term_def (id, vars, args, body), get_warnings ()
    end

  let decls st ?attr l =
    let env = typing_env st in
    let decls = T.decls env ?attr l in
    st, decls, get_warnings ()

  let terms st ?attr:_ l =
    let res = List.map (fun (t : Dolmen.Term.t) ->
        let env = typing_env ?loc:t.loc st in
        T.parse_term env t
      ) l in
    st, res, get_warnings ()

  let formula st ?attr:_ ~goal:_ (t : Dolmen.Term.t) =
    let env = typing_env ?loc:t.loc st in
    let res = T.parse env t in
    st, res, get_warnings ()

  let formulas st ?attr:_ l =
    let l' = List.map (fun (t : Dolmen.Term.t) ->
        let env = typing_env ?loc:t.loc st in
        T.parse env t
      ) l in
    st, l', get_warnings ()

end

