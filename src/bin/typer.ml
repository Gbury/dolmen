
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)


(* Shadowing *)
(* ************************************************************************ *)

let print_reason fmt r =
  match (r : Dolmen_type.Tff.reason) with
  | Inferred loc ->
    Format.fprintf fmt "inferred at %a" Dolmen.ParseLocation.fmt loc
  | Declared loc ->
    Format.fprintf fmt "declared at %a" Dolmen.ParseLocation.fmt loc

let print_shadowing_reasons fmt (id, old, cur) =
  Format.fprintf fmt "@[<v>Identifier %a is shadowed@ %a was %a@ and is now %a@]"
    Dolmen.Id.print id Dolmen.Id.print id
    print_reason old
    print_reason cur

(* Warnings *)
(* ************************************************************************ *)

module Warn = struct

  let shadowing_perm = ref State.Warn

  let binding_reason = function
    | `Not_found -> assert false
    | `Ty (_, reason)
    | `Cstr (_, reason)
    | `Term (_, reason) -> reason

  let shadow id old cur =
    match !shadowing_perm with
    | Allow -> ()
    | Warn ->
      State.warn () "%a"
        print_shadowing_reasons (id, binding_reason old, binding_reason cur)
    | Error ->
      raise (State.Shadowing (id, binding_reason old, binding_reason cur))

  let unused_ty_var loc v =
    State.warn () "@[<v>%a@ Type variable `%a` is unused@]"
      Dolmen.ParseLocation.fmt loc Dolmen.Expr.Print.ty_var v

  let unused_term_var loc v =
    State.warn () "@[<v>%a:@ Type variable `%a` is unused@]"
      Dolmen.ParseLocation.fmt loc Dolmen.Expr.Print.term_var v

  let error_in_attribute loc exn =
    State.warn () "@<v>%a:@ Error while type-checking an attribute:@ %s"
      Dolmen.ParseLocation.fmt loc (Printexc.to_string exn)

  let not_found id suggest =
    match suggest 3 with
    | [] -> ()
    | l ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      State.warn () "Looking up '%a' failed, possibilities were:@ @[<hov>%a@]"
        Dolmen.Id.print id (Format.pp_print_list ~pp_sep Dolmen.Id.print) l

  let superfluous_destructor _ _ _ _ =
    assert false

end

(* Dolmen_type functors instantiation *)
(* ************************************************************************ *)

module T = Dolmen_type.Tff.Make
    (Dolmen.Tag)
    (Dolmen.Expr.Ty)
    (Dolmen.Expr.Term)
    (Warn)

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
  | Ttype -> Format.fprintf fmt "Type"
  | Ty ty -> Format.fprintf fmt "the type@ %a" Dolmen.Expr.Ty.print ty
  | Term t -> Format.fprintf fmt "the term@ %a" Dolmen.Expr.Term.print t
  | Tags _ -> Format.fprintf fmt "some tags"

let print_opt pp fmt = function
  | None -> Format.fprintf fmt "<none>"
  | Some x -> pp fmt x

let report_error fmt = function
  (* Core Typechecking Errors *)
  | T.Infer_type_variable ->
    Format.fprintf fmt "@[<h>Cannot infer the type of a variable@]"
  | T.Expected (expect, got) ->
    Format.fprintf fmt "@[<hv>Expected %s but got %a@]" expect (print_opt print_res) got
  | T.Bad_op_arity (s, i, j) ->
    Format.fprintf fmt "@[<hv>Bad arity for builtin '%s':@ expected %d arguments but got %d@]" s j i
  | T.Bad_ty_arity (c, i) ->
    Format.fprintf fmt "@[<hv>Bad arity (got %d arguments) for type constant@ %a@]"
      i Dolmen.Expr.Print.ty_const c
  | T.Bad_cstr_arity (c, i, j) ->
    Format.fprintf fmt
      "@[<hv>Bad arity (got %d type argument, and %d term arguments) for term constructor@ %a@]"
      i j Dolmen.Expr.Print.term_const c
  | T.Bad_term_arity (c, i, j) ->
    Format.fprintf fmt
      "@[<hv>Bad arity (got %d type argument, and %d term arguments) for term constant@ %a@]"
      i j Dolmen.Expr.Print.term_const c
  | T.Var_application v ->
    Format.fprintf fmt "@[<hv>Cannot apply arguments to term variable@ %a@]" Dolmen.Expr.Print.id v
  | T.Ty_var_application v ->
    Format.fprintf fmt "@[<hv>Cannot apply arguments to type variable@ %a@]" Dolmen.Expr.Print.id v
  | T.Type_mismatch (t, expected) ->
    Format.fprintf fmt "@[<v>@[<v 2>The term:@ %a@]@ @ @[<v 2>has type@ %a@]@ @[<v 2>but was expected to be of type@ %a@]@]"
      Dolmen.Expr.Term.print t
      Dolmen.Expr.Ty.print (Dolmen.Expr.Term.ty t)
      Dolmen.Expr.Ty.print expected
  | T.Quantified_var_inference ->
    Format.fprintf fmt "@[<h>Cannot infer type for a quantified variable@]"
  | T.Unhandled_builtin b ->
    Format.fprintf fmt
      "@[<hv>The following Dolmen builtin is currently not handled@ %a@ Please report upstream@]"
      Dolmen.Term.print_builtin b
  | T.Cannot_tag_tag ->
    Format.fprintf fmt "@[<h>Cannot apply a tag to another tag (only expressions)@]"
  | T.Cannot_tag_ttype ->
    Format.fprintf fmt "@[<h>Cannot apply a tag to the Ttype constant@]"
  | T.Cannot_find id ->
    Format.fprintf fmt "@[<hv>Unbound identifier:@ '%a'@]" Dolmen.Id.print id
  | T.Type_var_in_type_constructor ->
    Format.fprintf fmt "@[<h>Type variables cannot appear in the signature of a type constant@]"
  | T.Missing_destructor id ->
    Format.fprintf fmt
      "@[<hv>The destructor '%a'@ was not provided by the user implementation.@ Please report upstream.@]"
      Dolmen.Id.print id
  | T.Higher_order_application ->
    Format.fprintf fmt "@[<h>Higher-order applications are not handled by the Tff typechecker@]"
  | T.Unbound_variables (tys, [], t) ->
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    Format.fprintf fmt "@[<v>In term:@ @[<hov>%a@]@ The following variables are not bound:@ @[<hov>%a@]@]"
      Dolmen.Expr.Term.print t (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) tys
  | T.Unbound_variables ([], ts, t) ->
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    Format.fprintf fmt "@[<v>In term:@ @[<hov>%a@]@ The following variables are not bound:@ @[<hov>%a@]@]"
      Dolmen.Expr.Term.print t (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) ts
  | T.Unbound_variables (tys, ts, t) ->
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    Format.fprintf fmt "@[<v>In term:@ %a@ The following variables are not bound:@ @[<hov>%a,@ %a@]@]"
      Dolmen.Expr.Term.print t
      (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) tys
      (Format.pp_print_list ~pp_sep Dolmen.Expr.Print.id) ts
  | T.Unhandled_ast ->
    Format.fprintf fmt "@[<hv>The typechecker did not know what to do with the term.@ Please report upstream.@]"

  (* Tptp Arithmetic errors *)
  | Tptp_Arith.Expected_arith_type ty ->
    Format.fprintf fmt "@[<hv>Arithmetic type expected but got@ %a@ %s@]"
      Dolmen.Expr.Ty.print ty
      "Tptp arithmetic symbols are only polymoprhic over the arithmetic types $int, $rat and $real."
  | Tptp_Arith.Cannot_apply_to ty ->
    Format.fprintf fmt "@[<hv>Cannot apply the arithmetic operation to type@ %a@]"
      Dolmen.Expr.Ty.print ty

  (* Smtlib Arithmetic errors *)
  | Smtlib_Reals_Ints.Expected_arith_type ty ->
    Format.fprintf fmt "@[<hv>Arithmetic type expected but got@ %a@ %s@]"
      Dolmen.Expr.Ty.print ty
      "The stmlib Reals_Ints theory requires an arithmetic type in order to correctly desugar the expression."

  (* Smtlib Bitvector errors *)
  | Smtlib_Bitv.Invalid_bin_char c ->
    Format.fprintf fmt "@[<hov>The@ character@ '%c'@ is@ invalid@ inside@ a@ binary@ bitvector@ litteral@]" c
  | Smtlib_Bitv.Invalid_hex_char c ->
    Format.fprintf fmt "@[<hov>The@ character@ '%c'@ is@ invalid@ inside@ a@ hexadecimal@ bitvector@ litteral@]" c

  (* Catch-all *)
  | _ ->
    Format.fprintf fmt "@[<hov>Unknown typing error,@ please report upstream, ^^@]"

(* Generate typing env from state *)
(* ************************************************************************ *)

(* TODO: set global options  to enforce limitations such as linearity. *)
let typing_env (st : State.t) =
  let expect =
    match st.input_lang with
    | Some Dimacs
    | Some ICNF
    | Some Tptp -> T.Typed Dolmen.Expr.Ty.prop
    | _ -> T.Nothing
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
        | None -> raise State.Missing_smtlib_logic
      in
      Dolmen_type.Base.smtlib_logic logic
        ~arrays:Smtlib_Arrays.parse
        ~bv:Smtlib_Bitv.parse
        ~core:Smtlib_Base.parse
        ~ints:Smtlib_Ints.parse
        ~reals:Smtlib_Reals.parse
        ~reals_ints:Smtlib_Reals_Ints.parse
  in
  let builtins = Dolmen_type.Base.merge [Def.parse; lang_builtins] in
  T.empty_env ~expect ?infer_base builtins


(* Wrappers around the Type-checking module *)
(* ************************************************************************ *)

let typecheck (st : State.t) = st.type_check

let def st ?attr id t =
  let env = typing_env st in
  begin match T.new_def ?attr env t id with
    | `Type_def (id, _, vars, body) ->
      let () = Def.define_ty id vars body in
      st, `Type_def (id, vars, body)
    | `Term_def (id, _, vars, args, body) ->
      let () = Def.define_term id vars args body in
      st, `Term_def (id, vars, args, body)
  end

let decl st ?attr id t =
  let env = typing_env st in
  st, T.new_decl ?attr env t id

let inductives st ?attr l =
  let env = typing_env st in
  st, T.inductives env ?attr l

let terms st ?attr:_ l =
  let env = typing_env st in
  st, List.map (T.parse_term env) l

let formula st ?attr:_ ~goal:_ t =
  let env = typing_env st in
  st, T.parse env t

let formulas st ?attr:_ l =
  let env = typing_env st in
  st, List.map (T.parse env) l



