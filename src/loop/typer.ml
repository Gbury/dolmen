
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
  module Smtlib_Base =
    Dolmen_type.Base.Smtlib.Tff(T)(Dolmen.Expr.Tags)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)
  module Smtlib_Ints =
    Dolmen_type.Arith.Smtlib.Int.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term.Int)
  module Smtlib_Reals =
    Dolmen_type.Arith.Smtlib.Real.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term.Real)
  module Smtlib_Reals_Ints =
    Dolmen_type.Arith.Smtlib.Real_Int.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)
  module Smtlib_Arrays =
    Dolmen_type.Arrays.Smtlib.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)
  module Smtlib_Bitv =
    Dolmen_type.Bitv.Smtlib.Tff(T)
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
      Format.fprintf fmt "Bad arity for builtin '%s':@ expected %d arguments but got %d" s j i
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
    | Smtlib_Reals_Ints.Expected_arith_type ty ->
      Format.fprintf fmt "Arithmetic type expected but got@ %a.@ %s"
        Dolmen.Expr.Ty.print ty
        "The stmlib Reals_Ints theory requires an arithmetic type in order to correctly desugar the expression."

    (* Smtlib Bitvector errors *)
    | Smtlib_Bitv.Invalid_bin_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a binary bitvector litteral" c
    | Smtlib_Bitv.Invalid_hex_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a hexadecimal bitvector litteral" c

    (* Catch-all *)
    | _ ->
      Format.fprintf fmt "Unknown typing error,@ please report upstream, ^^"

  let () =
    Printexc.register_printer (function
        | T.Typing_error (err, _, t) ->
          Some (Format.asprintf "@[<hv>While typing:@ @[<hov>%a@]@ %a@."
                  Dolmen.Term.print t
                  report_error err)
        | _ -> None
      )


  (* Generate typing env from state *)
  (* ************************************************************************ *)

  (* TODO: set global options  to enforce limitations such as linearity. *)
  let typing_env
      ?(loc=Dolmen.ParseLocation.mk "" 0 0 0 0)
      (st : (Parser.language, _, _) Dolmen.State.state) =
    let expect =
      match st.input_lang with
      | Some Dimacs
      | Some ICNF
      | Some Tptp ->
        T.Typed Dolmen.Expr.Ty.prop
      | _ ->
        T.Nothing
    in
    let infer_base =
      match st.input_lang with
      | Some Dimacs
      | Some ICNF -> Some Dolmen.Expr.Ty.prop
      | Some Tptp -> Some Dolmen.Expr.Ty.base
      | _ -> None
    in
    let lang_builtins =
      match st.input_lang with
      | None -> assert false
      | Some Dimacs
      | Some ICNF -> Dolmen_type.Base.noop
      | Some Alt_ergo -> Dolmen_type.Base.noop
      | Some Tptp ->
        Dolmen_type.Base.merge [
          Tptp_Base.parse;
          Tptp_Arith.parse;
        ]
      | Some Zf -> Zf_Base.parse
      | Some Smtlib ->
        let logic =
          match st.type_smtlib_logic with
          | Some s -> s
          | None -> S.missing_smtlib_logic ()
        in
        try
          (* Try and adequately combine the theories according
             to the smtlib logic *)
          Dolmen_type.Base.smtlib_logic logic
            ~arrays:Smtlib_Arrays.parse
            ~bv:Smtlib_Bitv.parse
            ~core:Smtlib_Base.parse
            ~ints:Smtlib_Ints.parse
            ~reals:Smtlib_Reals.parse
            ~reals_ints:Smtlib_Reals_Ints.parse
        with Dolmen_type.Base.Unknown_logic s ->
          (* Unknown logic, default to a reasonable combination *)
          add_warning loc (
            Format.asprintf "Unknown logic %s" s);
          Dolmen_type.Base.merge [
            Smtlib_Arrays.parse;
            Smtlib_Bitv.parse;
            Smtlib_Base.parse;
            Smtlib_Reals_Ints.parse;
          ]
    in
    let builtins = Dolmen_type.Base.merge [Def.parse; lang_builtins] in
    T.empty_env ~st:st.type_state ~expect ?infer_base builtins


  (* Wrappers around the Type-checking module *)
  (* ************************************************************************ *)

  let typecheck (st : _ Dolmen.State.state) = st.type_check

  let def st ?attr id t =
    let env = typing_env st in
    begin match T.new_def ?attr env t id with
      | `Type_def (id, _, vars, body) ->
        let () = Def.define_ty id vars body in
        st, `Type_def (id, vars, body), get_warnings ()
      | `Term_def (id, _, vars, args, body) ->
        let () = Def.define_term id vars args body in
        st, `Term_def (id, vars, args, body), get_warnings ()
    end

  let decls st ?attr l =
    let env = typing_env st in
    let decls = T.decls env ?attr l in
    st, decls, get_warnings ()

  let terms st ?attr:_ l =
    let env = typing_env st in
    let res = List.map (T.parse_term env) l in
    st, res, get_warnings ()

  let formula st ?attr:_ ~goal:_ t =
    let env = typing_env st in
    let res = T.parse env t in
    st, res, get_warnings ()

  let formulas st ?attr:_ l =
    let env = typing_env st in
    let l' = List.map (T.parse env) l in
    st, l', get_warnings ()

end

