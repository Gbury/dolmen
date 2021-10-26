
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Dolmen_type functors instantiation *)
(* ************************************************************************ *)

module T = Dolmen_type.Thf.Make
    (Dolmen.Std.Tag)(Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)

(* Definitions builtin *)
module Decl = Dolmen_type.Def.Declare(T)
module Subst = Dolmen_type.Def.Subst(T)(struct
    let of_list l =
      let aux acc (k, v) = Dolmen.Std.Expr.Subst.Var.bind acc k v in
      List.fold_left aux Dolmen.Std.Expr.Subst.empty l
    let ty_subst l ty =
      Dolmen.Std.Expr.Ty.subst (of_list l) ty
    let term_subst tys terms t =
      Dolmen.Std.Expr.Term.subst (of_list tys) (of_list terms) t
  end)

(* AE builtins *)
module Ae_Core =
  Dolmen_type.Core.Ae.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Ae_Arith =
  Dolmen_type.Arith.Ae.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Ae_Bitv =
  Dolmen_type.Bitv.Ae.Tff(T)
  (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Bitv)

(* Dimacs builtin *)
module Dimacs =
  Dolmen_type.Core.Dimacs.Tff(T)(Dolmen.Std.Expr.Term)

(* Tptp builtins *)
module Tptp_Core =
  Dolmen_type.Core.Tptp.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Tptp_Core_Ho =
  Dolmen_type.Core.Tptp.Thf(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Tptp_Arith =
  Dolmen_type.Arith.Tptp.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)

(* Stmlib theories *)
module Smtlib2_Core =
  Dolmen_type.Core.Smtlib2.Tff(T)(Dolmen.Std.Expr.Tags)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Smtlib2_Ints =
  Dolmen_type.Arith.Smtlib2.Int.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Int)
module Smtlib2_Reals =
  Dolmen_type.Arith.Smtlib2.Real.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Real)
module Smtlib2_Reals_Ints =
  Dolmen_type.Arith.Smtlib2.Real_Int.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Smtlib2_Arrays =
  Dolmen_type.Arrays.Smtlib2.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Smtlib2_Bitv =
  Dolmen_type.Bitv.Smtlib2.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Bitv)
module Smtlib2_Float =
  Dolmen_type.Float.Smtlib2.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Smtlib2_String =
  Dolmen_type.Strings.Smtlib2.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)

(* Zf *)
module Zf_Core =
  Dolmen_type.Core.Zf.Tff(T)(Dolmen.Std.Expr.Tags)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Zf_arith =
  Dolmen_type.Arith.Zf.Thf(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)

(* Printing helpers *)
(* ************************************************************************ *)

let pp_wrap pp fmt x =
  Format.fprintf fmt "`%a`" pp x

let print_symbol fmt symbol =
  match (symbol : T.symbol) with
  | Id id -> Dolmen.Std.Id.print fmt id
  | Builtin builtin -> Dolmen.Std.Term.print_builtin fmt builtin

let print_res fmt res =
  match (res : T.res) with
  | T.Ttype -> Format.fprintf fmt "Type"
  | T.Ty ty ->
    Format.fprintf fmt "the type@ %a" (pp_wrap Dolmen.Std.Expr.Ty.print) ty
  | T.Term t ->
    Format.fprintf fmt "the term@ %a" (pp_wrap Dolmen.Std.Expr.Term.print) t
  | T.Tags _ -> Format.fprintf fmt "some tags"

let print_opt pp fmt = function
  | None -> Format.fprintf fmt "<none>"
  | Some x -> pp fmt x

let rec print_expected fmt = function
  | [] -> assert false
  | x :: [] -> Format.fprintf fmt "%d" x
  | x :: r -> Format.fprintf fmt "%d or %a" x print_expected r

let print_fragment (type a) fmt (env, fragment : T.env * a T.fragment) =
  match fragment with
  | T.Ast ast -> pp_wrap Dolmen.Std.Term.print fmt ast
  | T.Def d -> Dolmen.Std.Statement.print_def fmt d
  | T.Decl d -> Dolmen.Std.Statement.print_decl fmt d
  | T.Defs d ->
    Dolmen.Std.Statement.print_group Dolmen.Std.Statement.print_def fmt d
  | T.Decls d ->
    Dolmen.Std.Statement.print_group Dolmen.Std.Statement.print_decl fmt d
  | T.Located _ ->
    let full = T.fragment_loc env fragment in
    let loc = Dolmen.Std.Loc.full_loc full in
    Format.fprintf fmt "<located at %a>" Dolmen.Std.Loc.fmt loc

let decl_loc d =
  match (d : Dolmen.Std.Statement.decl) with
  | Record { loc; _ }
  | Abstract { loc; _ }
  | Inductive { loc; _ } -> loc

let print_bound_kind fmt = function
  | `Quantified -> Format.fprintf fmt "quantified"
  | `Letbound -> Format.fprintf fmt "let-bound"

let print_reason fmt r =
  match (r : T.reason) with
  | Builtin ->
    Format.fprintf fmt "defined by a builtin theory"
  | Bound (file, ast) ->
    Format.fprintf fmt "bound at %a"
      Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file ast.loc)
  | Inferred (file, ast) ->
    Format.fprintf fmt "inferred at %a"
      Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file ast.loc)
  | Defined (file, d) ->
    Format.fprintf fmt "defined at %a"
      Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file d.loc)
  | Declared (file, d) ->
    Format.fprintf fmt "declared at %a"
      Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file (decl_loc d))

let print_reason_opt fmt = function
  | Some r -> print_reason fmt r
  | None -> Format.fprintf fmt "<location missing>"

let rec print_wildcard_origin fmt = function
  | T.Arg_of src
  | T.Ret_of src -> print_wildcard_origin fmt src
  | T.From_source _ast ->
    Format.fprintf fmt "the@ contents@ of@ a@ source@ wildcard"
  | T.Added_type_argument _ast ->
    Format.fprintf fmt "the@ implicit@ type@ to@ provide@ to@ an@ application"
  | T.Symbol_inference { symbol; symbol_loc = _; inferred_ty; } ->
    Format.fprintf fmt
      "the@ type@ for@ the@ symbol@ %a@ to@ be@ %a"
      (pp_wrap Dolmen.Std.Id.print) symbol
      (pp_wrap Dolmen.Std.Expr.Ty.print) inferred_ty
  | T.Variable_inference { variable; variable_loc = _; inferred_ty; } ->
    Format.fprintf fmt
      "the@ type@ for@ the@ quantified@ variable@ %a@ to@ be@ %a"
      (pp_wrap Dolmen.Std.Id.print) variable
      (pp_wrap Dolmen.Std.Expr.Ty.print) inferred_ty

let rec print_wildcard_path env fmt = function
  | T.Arg_of src ->
    Format.fprintf fmt "one@ of@ the@ argument@ types@ of@ %a"
      (print_wildcard_path env) src
  | T.Ret_of src ->
    Format.fprintf fmt "the@ return@ type@ of@ %a"
      (print_wildcard_path env) src
  | _ ->
    Format.fprintf fmt "that@ type"

let rec print_wildcard_loc env fmt = function
  | T.Arg_of src
  | T.Ret_of src -> print_wildcard_loc env fmt src
  | T.From_source ast ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env ast.loc) in
    Format.fprintf fmt
      "The@ source@ wildcard@ is@ located@ at@ %a"
      Dolmen.Std.Loc.fmt_pos loc
  | T.Added_type_argument ast ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env ast.loc) in
    Format.fprintf fmt
      "The@ application@ is@ located@ at@ %a"
      Dolmen.Std.Loc.fmt_pos loc
  | T.Symbol_inference { symbol; symbol_loc; inferred_ty = _; } ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env symbol_loc) in
    Format.fprintf fmt
      "Symbol@ %a@ is@ located@ be@ %a"
      (pp_wrap Dolmen.Std.Id.print) symbol
      Dolmen.Std.Loc.fmt_pos loc
  | T.Variable_inference { variable; variable_loc; inferred_ty = _; } ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env variable_loc) in
    Format.fprintf fmt
      "Variable@ %a@ is@ bound@ at@ %a"
      (pp_wrap Dolmen.Std.Id.print) variable
      Dolmen.Std.Loc.fmt_pos loc

let rec print_wildcard_origin_loc env fmt = function
  | T.Arg_of src
  | T.Ret_of src -> print_wildcard_origin_loc env fmt src
  | T.From_source ast ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env ast.loc) in
    Format.fprintf fmt
      "a@ source@ wildcard@ located@ at@ %a"
      Dolmen.Std.Loc.fmt_pos loc
  | T.Added_type_argument ast ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env ast.loc) in
    Format.fprintf fmt
      "the@ implicit@ type@ argument@ to@ provide@ to@ \
       the@ polymorphic@ application@ at@ %a"
      Dolmen.Std.Loc.fmt_pos loc
  | T.Symbol_inference { symbol; symbol_loc; inferred_ty = _; } ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env symbol_loc) in
    Format.fprintf fmt
      "the@ type@ for@ the@ symbol@ %a,@ located@ at@ %a"
      (pp_wrap Dolmen.Std.Id.print) symbol
      Dolmen.Std.Loc.fmt_pos loc
  | T.Variable_inference { variable; variable_loc; inferred_ty = _; } ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env variable_loc) in
    Format.fprintf fmt
      "the@ type@ for@ the@ variable@ %a,@ located@ at@ %a"
      (pp_wrap Dolmen.Std.Id.print) variable
      Dolmen.Std.Loc.fmt_pos loc




(* Hint printers *)
(* ************************************************************************ *)

let fo_hint _ =
  Some (
    Format.dprintf "%a" Format.pp_print_text
      "This statement was parsed as a first-order statement")

let text_hint = function
  | "" -> None
  | msg -> Some (Format.dprintf "%a" Format.pp_print_text msg)

let poly_hint (c, expected, actual) =
  let n_ty, n_t = Dolmen.Std.Expr.Term.Const.arity c in
  let total_arity = n_ty + n_t in
  match expected with
  | [x] when x = total_arity && actual = n_t ->
    Some (
      Format.dprintf "%a" Format.pp_print_text
        "the head of the application is polymorphic, \
         you probably forgot the type arguments@]")
  | [x] when x = n_t && n_ty <> 0 ->
    Some (
      Format.dprintf "%a" Format.pp_print_text
        "it looks like the language enforces implicit polymorphism, \
         i.e. no type arguments are to be provided to applications \
         (and instead type annotation/coercions should be used).")
  | _ :: _ :: _ ->
    Some (
      Format.dprintf "%a" Format.pp_print_text
        "this is a polymorphic function, and multiple accepted arities \
         are possible because the language supports inference of all type \
         arguments when none are given in an application.")
  | _ -> None

let literal_hint b id =
  if not b then None else
  match (id : Dolmen.Std.Id.t) with
  | { ns = Value Integer; name = Simple _; } ->
    Some (
      Format.dprintf "%a" Format.pp_print_text
        "The current logic does not include integer arithmtic")
  | { ns = Value Rational; name = Simple _; } ->
    Some (
      Format.dprintf "%a" Format.pp_print_text
        "The current logic does not include rational arithmtic")
  | { ns = Value Real; name = Simple _; } ->
    Some (
      Format.dprintf "%a" Format.pp_print_text
        "The current logic does not include real arithmtic")
  | { ns = Term; name = Indexed { basename = s; indexes = _; }; }
    when (String.length s >= 2 && s.[0] = 'b' && s.[1] = 'v') ->
    Some (
      Format.dprintf "%a" Format.pp_print_text
        "The current logic does not include extended bitvector literals")
  | _ -> None

let poly_arg_hint _ =
  Some (
    Format.dprintf "%a" Format.pp_print_text
      "The typechecker enforces prenex/rank-1 polymorphism. \
       In languages with explicit type arguments for polymorphic functions, \
       you must apply this term to the adequate number of type arguments to \
       make it monomorph.")

let poly_param_hint _ =
  Some (
    Format.dprintf "%a" Format.pp_print_text
      "The typechecker enforces prenex/rank-1 polymorphism. \
       This means that only monomorphic types can appear as
       parameters of a function type.")


(* Typing warnings *)
(* ************************************************************************ *)

let code = Code.typing

let unused_type_variable =
  Report.Warning.mk ~code ~mnemonic:"unused-type-var"
    ~message:(fun fmt (kind, v) ->
        Format.fprintf fmt
          "The following %a type variable is unused: '%a'"
          print_bound_kind kind Dolmen.Std.Expr.Print.id v)
    ~name:"Unused bound type variable" ()

let unused_term_variable =
  Report.Warning.mk ~code ~mnemonic:"unused-term-var"
    ~message:(fun fmt (kind, v) ->
        Format.fprintf fmt
          "The following %a term variable is unused: `%a`"
          print_bound_kind kind Dolmen.Std.Expr.Print.id v)
    ~name:"Unused bound term variable" ()

let error_in_attribute =
  Report.Warning.mk ~code ~mnemonic:"error-in-attr"
    ~message:(fun fmt exn ->
        Format.fprintf fmt
          "Exception while typing attribute:@ %s"
          (Printexc.to_string exn))
    ~name:"Exception while typing an attribute" ()

let superfluous_destructor =
  Report.Warning.mk ~code:Code.bug ~mnemonic:"extra-dstr"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Superfluous destructor returned by term implementation")
    ~name:"Superfluous destructor" ()

let shadowing =
  Report.Warning.mk ~code ~mnemonic:"shadowing"
    ~message:(fun fmt (id, old) ->
        Format.fprintf fmt
          "Shadowing: %a was already %a"
          (pp_wrap Dolmen.Std.Id.print) id
          print_reason_opt (T.binding_reason old))
    ~name:"Shadowing of identifier" ()

let almost_linear =
  Report.Warning.mk ~code ~mnemonic:"almost-linear-expr"
    ~message:(fun fmt _ ->
        Format.fprintf fmt
          "This is a non-linear expression according to the smtlib spec.")
    ~hints:[text_hint]
    ~name:"Non-linear expression in linear arithmetic" ()

let logic_reset =
  Report.Warning.mk ~code ~mnemonic:"logic-reset"
    ~message:(fun fmt old_loc ->
        Format.fprintf fmt "Logic was already set at %a"
          Dolmen.Std.Loc.fmt_pos old_loc)
    ~name:"Multiple set-logic statements" ()

let unknown_logic =
  Report.Warning.mk ~code ~mnemonic:"unknown-logic"
    ~message:(fun fmt s ->
        Format.fprintf fmt "Unknown logic: %s" s)
    ~name:"Unknown logic" ()

let set_logic_not_supported =
  Report.Warning.mk ~code ~mnemonic:"set-logic-ignored"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Set logic is not supported for the current language")
    ~name:"Set logic not supported for current language" ()

let unknown_warning =
  Report.Warning.mk ~code:Code.bug ~mnemonic:"unknown-warning"
    ~message:(fun fmt cstr_name ->
        Format.fprintf fmt
          "@[<v>Unknown warning:@ %s@ please report upstream, ^^@]" cstr_name)
    ~name:"Unknown warning" ()

(* Typing errors *)
(* ************************************************************************ *)

let not_well_founded_datatype =
  Report.Error.mk ~code ~mnemonic:"wf-datatype"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Not well founded datatype declaration")
    ~name:"Not Well Founded Datatype" ()

let expect_error =
  Report.Error.mk ~code ~mnemonic:"typing-bad-kind"
    ~message:(fun fmt (expected, got) ->
        Format.fprintf fmt "Expected %s but got %a"
          expected (print_opt print_res) got)
    ~name:"Bad kind" ()

let bad_index_arity =
  Report.Error.mk ~code ~mnemonic:"bad-index-arity"
    ~message:(fun fmt (s, expected, actual) ->
        Format.fprintf fmt
          "The indexed family of operators '%s' expects %d indexes, but was given %d"
          s expected actual)
    ~name:"Incorrect arity for indexed operator" ()

let bad_type_arity =
  Report.Error.mk ~code ~mnemonic:"bad-type-arity"
    ~message:(fun fmt (c, actual) ->
        Format.fprintf fmt "Bad arity: got %d arguments for type constant@ %a"
          actual Dolmen.Std.Expr.Print.ty_cst c)
    ~name:"Incorrect Arity for type constant application" ()

let bad_op_arity =
  Report.Error.mk ~code ~mnemonic:"bad-op-arity"
    ~message:(fun fmt (symbol, expected, actual) ->
        Format.fprintf fmt
          "Bad arity for symbol '%a':@ expected %a arguments but got %d"
          print_symbol symbol print_expected expected actual)
    ~name:"Incorrect arity for operator application" ()

let bad_cstr_arity =
  Report.Error.mk ~code ~mnemonic:"bad-cstr-arity"
    ~hints:[poly_hint]
    ~message:(fun fmt (c, expected, actual) ->
        Format.fprintf fmt
          "Bad arity: expected %a arguments but got %d arguments for constructor@ %a"
          print_expected expected actual Dolmen.Std.Expr.Print.term_cst c)
    ~name:"Incorrect arity for constructor application" ()

let bad_term_arity =
  Report.Error.mk ~code ~mnemonic:"bad-term-arity"
    ~hints:[poly_hint]
    ~message:(fun fmt (c, expected, actual) ->
        Format.fprintf fmt
          "Bad arity: expected %a but got %d arguments for function@ %a"
          print_expected expected actual Dolmen.Std.Expr.Print.term_cst c)
    ~name:"Incorrect arity for term application" ()

let bad_poly_arity =
  Report.Error.mk ~code ~mnemonic:"bad-poly-arity"
    ~message:(fun fmt (vars, args) ->
        let expected = List.length vars in
        let provided = List.length args in
        if provided > expected then
          (* Over application *)
          Format.fprintf fmt
            "This@ function@ expected@ at@ most@ %d@ type@ arguments,@ \
             but@ was@ here@ provided@ with@ %d@ type@ arguments."
            expected provided
        else
          (* under application *)
          Format.fprintf fmt
            "This@ function@ expected@ exactly@ %d@ type@ arguments,@ \
             since@ term@ arguments@ are@ also@ provided,@ but@ was@ given@ \
             %d@ arguments."
            expected provided)
    ~name:"Incorrect arity for type arguments of a term application" ()

let over_application =
  Report.Error.mk ~code ~mnemonic:"over-application"
    ~message:(fun fmt over_args ->
        let over = List.length over_args in
        Format.fprintf fmt
          "Over application:@ this@ application@ has@ %d@ \
           too@ many@ term@ arguments." over)
    ~name:"Too many arguments for an application" ()

let repeated_record_field =
  Report.Error.mk ~code ~mnemonic:"repeated-field"
    ~message:(fun fmt f ->
        Format.fprintf fmt
          "The field %a is used more than once in this record construction"
          Dolmen.Std.Expr.Print.id f)
    ~name:"Repeated field in a record construction" ()

let missing_record_field =
  Report.Error.mk ~code ~mnemonic:"missing-field"
    ~message:(fun fmt f ->
        Format.fprintf fmt
          "The field %a is missing from this record construction"
          Dolmen.Std.Expr.Print.id f)
    ~name:"Missing field in a record construction" ()

let mismatch_record_type =
  Report.Error.mk ~code ~mnemonic:"mismatch-field"
    ~message:(fun fmt (f, r) ->
        Format.fprintf fmt
          "The field %a does not belong to record type %a"
          Dolmen.Std.Expr.Print.id f Dolmen.Std.Expr.Print.id r)
    ~name:"Field of another record type in a record construction" ()

let ty_var_application =
  Report.Error.mk ~code ~mnemonic:"type-var-app"
    ~message:(fun fmt v ->
        Format.fprintf fmt
          "Cannot apply arguments to type variable@ %a" Dolmen.Std.Expr.Print.id v)
    ~name:"Application of a type variable" ()

let var_application =
  Report.Error.mk ~code ~mnemonic:"term-var-app"
    ~hints:[fo_hint]
    ~message:(fun fmt v ->
        Format.fprintf fmt
          "Cannot apply arguments to term variable@ %a" Dolmen.Std.Expr.Print.id v)
    ~name:"Application of a term variable" ()

let type_mismatch =
  Report.Error.mk ~code ~mnemonic:"type-mismatch"
    ~message:(fun fmt (t, expected) ->
        Format.fprintf fmt "The term:@ %a@ has type@ %a@ but was expected to be of type@ %a"
          (pp_wrap Dolmen.Std.Expr.Term.print) t
          (pp_wrap Dolmen.Std.Expr.Ty.print) (Dolmen.Std.Expr.Term.ty t)
          (pp_wrap Dolmen.Std.Expr.Ty.print) expected)
    ~name:"Incorrect argument type in an application" ()

let quant_var_inference =
  Report.Error.mk ~code ~mnemonic:"quant-var-inference"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Cannot infer type for a quantified variable")
    ~name:"Inference of a quantified variable's type" ()

let unhandled_builtin =
  Report.Error.mk ~code:Code.bug ~mnemonic:"unhandled-builtin"
    ~message:(fun fmt b ->
        Format.fprintf fmt
          "The following Dolmen builtin is currently not handled@ %a.@ Please report upstream"
          (pp_wrap Dolmen.Std.Term.print_builtin) b)
    ~name:"Unhandled builtin in typechecking" ()

let cannot_tag_tag =
  Report.Error.mk ~code:Code.bug ~mnemonic:"tag-tag"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Cannot apply a tag to another tag (only expressions)")
    ~name:"Trying to tag a tag" ()

let cannot_tag_ttype =
  Report.Error.mk ~code:Code.bug ~mnemonic:"tag-ttype"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Cannot apply a tag to the Ttype constant")
    ~name:"Tying to tag Ttype" ()

let unbound_identifier =
  Report.Error.mk ~code ~mnemonic:"unbound-id"
    ~message:(fun fmt (id, _, _) ->
        Format.fprintf fmt "Unbound identifier:@ %a"
          (pp_wrap Dolmen.Std.Id.print) id)
    ~hints:[
      (fun (id, _, lit_hint) -> literal_hint lit_hint id);
      (fun (_, msg, _) -> text_hint msg);]
    ~name:"Unbound identifier" ()

let multiple_declarations =
  Report.Error.mk ~code ~mnemonic:"redeclaration"
    ~message:(fun fmt (id, old) ->
        Format.fprintf fmt
          "Duplicate declaration of %a, which was already %a"
          (pp_wrap Dolmen.Std.Id.print) id
          print_reason_opt (T.binding_reason old))
    ~name:"Multiple declarations of the same symbol" ()

let forbidden_quant =
  Report.Error.mk ~code ~mnemonic:"forbidden-quant"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Quantified expressions are forbidden by the logic.")
    ~name:"Forbidden quantifier" ()

let type_var_in_type_cstr =
  Report.Error.mk ~code ~mnemonic:"type-var-in-type-cstr"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Type variables cannot appear in the signature of a type constant")
    ~name:"Type variable in the isgnature of a type constructor" ()

let missing_destructor =
  Report.Error.mk ~code:Code.bug ~mnemonic:"missing-destructor"
    ~message:(fun fmt id ->
        Format.fprintf fmt
          "The destructor %a@ was not provided by the user implementation.@ Please report upstream."
          (pp_wrap Dolmen.Std.Id.print) id)
    ~name:"Missing destructor in implementation" ()

let higher_order_app =
  Report.Error.mk ~code ~mnemonic:"ho-app"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Higher-order applications are not handled by the Tff typechecker")
    ~hints:[fo_hint]
    ~name:"Higher-order application" ()

let higher_order_type =
  Report.Error.mk ~code ~mnemonic:"ho-type"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Higher-order types are not handled by the Tff typechecker")
    ~hints:[fo_hint]
    ~name:"Higher-order type" ()

let higher_order_env_in_tff_typer =
  Report.Error.mk ~code:Code.bug ~mnemonic:"ho-env-in-tff"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Programmer error: trying to create a typing env for \
           higher-order with the first-order typechecker.")
    ~name:"Higher order env in TFF type-checker" ()

let poly_arg =
  Report.Error.mk ~code ~mnemonic:"poly-arg"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Polymorphic terms cannot be given as argument of a function.")
    ~hints:[poly_arg_hint]
    ~name:"Polymorphic argument in an application" ()

let non_prenex_polymorphism =
  Report.Error.mk ~code ~mnemonic:"poly-param"
    ~message:(fun fmt ty ->
        Format.fprintf fmt "The following polymorphic type occurs in a \
                            non_prenex position: %a"
          (pp_wrap Dolmen.Std.Expr.Ty.print) ty)
    ~hints:[poly_param_hint]
    ~name:"Polymorphic function parameter" ()

let inference_forbidden =
  Report.Error.mk ~code ~mnemonic:"inference-forbidden"
    ~message:(fun fmt (env, w_src, inferred_ty) ->
        Format.fprintf fmt
          "@[<v>@[<hov>The@ typechecker@ inferred@ %a.@]@ \
           @[<hov>That@ inference@ lead@ to@ infer@ %a@ to@ be@ %a.@]@ \
           @[<hov>However,@ the@ language@ specified@ inference@ \
           at@ that@ point@ was@ forbidden@]@ \
           @[<hov>%a@]\
           @]"
          print_wildcard_origin w_src
          (print_wildcard_path env) w_src
          (pp_wrap Dolmen.Std.Expr.Ty.print) inferred_ty
          (print_wildcard_loc env) w_src)
    ~name:"Forbidden type inference" ()

let inference_conflict =
  Report.Error.mk ~code ~mnemonic:"inference-conflict"
    ~message:(fun fmt (env, w_src, inferred_ty, allowed_tys) ->
        Format.fprintf fmt
          "@[<v>@[<hov>The@ typechecker@ inferred@ %a.@]@ \
           @[<hov>That@ inference@ lead@ to@ infer@ %a@ to@ be@ %a.@]@ \
           @[<hov>However,@ the@ language@ specified@ that@ only@ the@ following@ \
           types@ should@ be@ allowed@ there:@ %a@]@ \
           @[<hov>%a@]\
           @]"
          print_wildcard_origin w_src
          (print_wildcard_path env) w_src
          (pp_wrap Dolmen.Std.Expr.Ty.print) inferred_ty
          (Format.pp_print_list (pp_wrap Dolmen.Std.Expr.Ty.print)
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")) allowed_tys
          (print_wildcard_loc env) w_src)
    ~name:"Conflict in type inference" ()

let inference_scope_escape =
  Report.Error.mk ~code ~mnemonic:"inference-scope-escape"
    ~message:(fun fmt (env, w_src, escaping_var, var_reason) ->
        Format.fprintf fmt
          "@[<v>@[<hov>The@ typechecker@ inferred@ %a.@]@ \
           @[<hov>That@ inference@ lead@ to@ infer@ %a@ to@ contain@ \
           the@ variable@ %a@ which@ is@ not@ in@ the@ scope@ \
           of@ the@ inferred@ type.@]@ \
           @[<hov>%a@]@ \
           @[<hov>Variable %a is@ %a.@]\
           @]"
          print_wildcard_origin w_src
          (print_wildcard_path env) w_src
          (pp_wrap Dolmen.Std.Expr.Ty.Var.print) escaping_var
          (print_wildcard_loc env) w_src
          (pp_wrap Dolmen.Std.Expr.Ty.Var.print) escaping_var
          print_reason_opt var_reason)
    ~name:"Scope escape from a type due to inference" ()

let unbound_type_wildcards =
  Report.Error.mk ~code ~mnemonic:"inference-incomplete"
    ~message:(fun fmt (env, l) ->
        let pp_sep fmt () = Format.fprintf fmt "@ " in
        let pp_src fmt src =
          Format.fprintf fmt "@[<hov>%a@]" (print_wildcard_origin_loc env) src
        in
        let pp_wild fmt (w, srcs) =
          Format.fprintf fmt "%a, @[<v>%a@]"
            (pp_wrap Dolmen.Std.Expr.Print.id) w
            (Format.pp_print_list ~pp_sep pp_src) srcs
        in
        Format.fprintf fmt
          "@[<v 2>@[<hov>%a@]:@ %a@]"
          Format.pp_print_text
          "Top-level formulas should be closed, but the following type variables are free"
          (Format.pp_print_list ~pp_sep pp_wild) l
      )
    ~name:"Under-specified type inference" ()

let unhandled_ast : (T.env * Dolmen_std.Term.t T.fragment) Report.Error.t =
  Report.Error.mk ~code ~mnemonic:"unhandled-ast"
    ~message:(fun fmt (env, fragment) ->
        Format.fprintf fmt
          "The typechecker did not know what to do with the following term.@ \
           Please report upstream.@\n%a"
          print_fragment (env, fragment))
    ~name:"Unhandled AST fragment" ()

let expected_arith_type =
  Report.Error.mk ~code ~mnemonic:"arith-type-expected"
    ~message:(fun fmt (ty, _) ->
        Format.fprintf fmt "Arithmetic type expected but got@ %a.@ %s"
          (pp_wrap Dolmen.Std.Expr.Ty.print) ty
          "Tptp arithmetic symbols are only polymorphic over the arithmetic \
           types $int, $rat and $real.")
    ~hints:[(fun (_, msg) -> text_hint msg)]
    ~name:"Non-arithmetic use of overloaded arithmetic function" ()

let expected_specific_arith_type =
  Report.Error.mk ~code ~mnemonic:"arith-type-specific"
    ~message:(fun fmt ty ->
        Format.fprintf fmt "Cannot apply the arithmetic operation to type@ %a"
          (pp_wrap Dolmen.Std.Expr.Ty.print) ty)
    ~name:"Incorrect use of overloaded arithmetic function" ()

let forbidden_array_sort =
  Report.Error.mk ~code ~mnemonic:"forbidden-array-sort"
    ~message:(fun fmt _ ->
        Format.fprintf fmt "Forbidden array sort.")
    ~hints:[text_hint]
    ~name:"Forbidden array sort" ()

let non_linear_expression =
  Report.Error.mk ~code ~mnemonic:"non-linear-expr"
    ~message:(fun fmt _ ->
        Format.fprintf fmt "Non-linear expressions are forbidden by the logic.")
    ~hints:[text_hint]
    ~name:"Non linear expression in linear arithmetic logic" ()

let invalid_bin_bitvector_char =
  Report.Error.mk ~code ~mnemonic:"invalid-bv-bin-char"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The character '%c' is invalid inside a binary bitvector litteral" c)
    ~name:"Invalid character in a binary bitvector literal" ()

let invalid_hex_bitvector_char =
  Report.Error.mk ~code ~mnemonic:"invalid-bv-hex-char"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The character '%c' is invalid inside an hexadecimal bitvector litteral" c)
    ~name:"Invalid character in an hexadecimal bitvector literal" ()

let invalid_dec_bitvector_char =
  Report.Error.mk ~code ~mnemonic:"invalid-bv-dec-char"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The character '%c' is invalid inside a decimal bitvector litteral" c)
    ~name:"Invalid character in a decimal bitvector literal" ()

let invalid_hex_string_char =
  Report.Error.mk ~code ~mnemonic:"invalid-hex-string-char"
    ~message:(fun fmt s ->
        Format.fprintf fmt
          "The following is not a valid hexadecimal character: '%s'" s)
    ~name:"Invalid hexadecimal character in a string literal" ()

let invalid_string_char =
  Report.Error.mk ~code ~mnemonic:"invalid-string-char"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The following character is not allowed in string literals: '%c'" c)
    ~name:"Invalid character in a string literal" ()

let invalid_string_escape_sequence =
  Report.Error.mk ~code ~mnemonic:"invalid-string-escape"
    ~message:(fun fmt (s, i) ->
        Format.fprintf fmt
          "The escape sequence starting at index %d in the \
           following string is not allowed: '%s'" i s)
    ~name:"Invalid escape sequence in a string literal" ()

let bad_tptp_kind =
  Report.Error.mk ~code ~mnemonic:"bad-tptp-kind"
    ~message:(fun fmt o ->
        match o with
        | None ->
          Format.fprintf fmt "Missing kind for the tptp statement."
        | Some s ->
          Format.fprintf fmt "Unknown kind for the tptp statement: '%s'." s)
    ~name:"Invalid kind for a TPTP statement" ()

let missing_smtlib_logic =
  Report.Error.mk ~code ~mnemonic:"missing-smt-logic"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Missing logic (aka set-logic for smtlib2).")
    ~name:"Missing set-logic in an SMTLIB file" ()

let illegal_decl =
  Report.Error.mk ~code ~mnemonic:"illegal-decl"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Illegal declaration.")
    ~name:"Illegal declaration in a file" ()

let invalid_push =
  Report.Error.mk ~code ~mnemonic:"invalid-push"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Invalid push payload (payload must be positive)")
    ~name:"Negative payload for a push statement" ()

let invalid_pop =
  Report.Error.mk ~code ~mnemonic:"invalid-pop"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Invalid pop payload (payload must be positive)")
    ~name:"Negative payload for a pop statement" ()

let empty_pop =
  Report.Error.mk ~code ~mnemonic:"empty-pop"
    ~message:(fun fmt () ->
        Format.fprintf fmt
          "Pop instruction with an empty stack (likely a \
           result of a missing push or excessive pop)")
    ~name:"Excessive use of pop leading to an empty stack" ()

let unknown_error =
  Report.Error.mk ~code:Code.bug ~mnemonic:"unknown-typing-error"
    ~message:(fun fmt cstr_name ->
      Format.fprintf fmt
        "@[<v>Unknown typing error:@ %s@ please report upstream, ^^@]"
        cstr_name)
    ~name:"Unknown typing error" ()


(* Typing state *)
(* ************************************************************************ *)

(* This is here to define the typing state (not to confuse with the state
   passed in the pipes, which will contain the typing state. *)

type ty_state = {
  (* logic used *)
  logic : Dolmen_type.Logic.t;
  logic_loc : Dolmen.Std.Loc.loc;
  (* current typechecker global state *)
  typer : T.state;
  (* typechecker state stack *)
  stack : T.state list;
}

let new_state () = {
  logic = Auto;
  logic_loc = Dolmen.Std.Loc.dummy;
  typer = T.new_state ();
  stack = [];
}


(* Make functor *)
(* ************************************************************************ *)

module type S = Typer_intf.S

module Make(S : State_intf.Typer with type ty_state := ty_state) = struct

  (* New warnings & errors *)
  (* ************************************************************************ *)

  type _ T.err +=
    | Bad_tptp_kind : string option -> Dolmen.Std.Loc.t T.err
    | Missing_smtlib_logic : Dolmen.Std.Loc.t T.err
    | Illegal_decl : Dolmen.Std.Statement.decl T.err
    | Invalid_push_n : Dolmen.Std.Loc.t T.err
    | Invalid_pop_n : Dolmen.Std.Loc.t T.err
    | Pop_with_empty_stack : Dolmen.Std.Loc.t T.err

  (* Report type warnings *)
  (* ************************************************************************ *)

  let smtlib2_6_shadow_rules st =
    match (S.input_lang st : Logic.language option) with
    | Some Smtlib2 (`Latest | `V2_6 | `Poly) -> true
    | _ -> false

  let report_warning st (T.Warning (env, fragment, warn)) =
    let loc = T.fragment_loc env fragment in
    match warn with
    (* typer warnings that are actually errors given some languages spec *)
    | T.Shadowing (id, ((`Builtin `Term | `Not_found) as old), `Variable _)
    | T.Shadowing (id, ((`Constant _ | `Builtin _ | `Not_found) as old), `Constant _)
      when smtlib2_6_shadow_rules st ->
      S.error ~loc st multiple_declarations (id, old)

    (* warnings *)
    | T.Unused_type_variable (kind, v) ->
      S.warn ~loc st unused_type_variable (kind, v)
    | T.Unused_term_variable (kind, v) ->
      S.warn ~loc st unused_term_variable (kind, v)
    | T.Error_in_attribute exn ->
      S.warn ~loc st error_in_attribute exn
    | T.Superfluous_destructor _ ->
      S.warn ~loc st superfluous_destructor ()
    | T.Shadowing (id, old, _cur) ->
      S.warn ~loc st shadowing (id, old)
    | Smtlib2_Ints.Restriction msg
    | Smtlib2_Reals.Restriction msg
    | Smtlib2_Reals_Ints.Restriction msg ->
      S.warn ~loc st almost_linear msg
    | _ ->
      S.warn ~loc st unknown_warning
          (Obj.Extension_constructor.(name (of_val warn)))

  (* Report type errors *)
  (* ************************************************************************ *)

  let report_error st (T.Error (env, fragment, err)) =
    let loc = T.fragment_loc env fragment in
    match err with
    (* Datatype definition not well founded *)
    | T.Not_well_founded_datatypes _ ->
      S.error ~loc st not_well_founded_datatype ()
    (* Generic error for when something was expected but not there *)
    | T.Expected (expect, got) ->
      S.error ~loc st expect_error (expect, got)
      (* Arity errors *)
    | T.Bad_index_arity (s, expected, actual) ->
      S.error ~loc st bad_index_arity (s, expected, actual)
    | T.Bad_ty_arity (c, actual) ->
      S.error ~loc st bad_type_arity (c, actual)
    | T.Bad_op_arity (symbol, expected, actual) ->
      S.error ~loc st bad_op_arity (symbol, expected, actual)
    | T.Bad_cstr_arity (c, expected, actual) ->
      S.error ~loc st bad_cstr_arity (c, expected, actual)
    | T.Bad_term_arity (c, expected, actual) ->
      S.error ~loc st bad_term_arity (c, expected, actual)
    | T.Bad_poly_arity (vars, args) ->
      S.error ~loc st bad_poly_arity (vars, args)
    | T.Over_application over_args ->
      S.error ~loc st over_application over_args
    (* Record constuction errors *)
    | T.Repeated_record_field f ->
      S.error ~loc st repeated_record_field f
    | T.Missing_record_field f ->
      S.error ~loc st missing_record_field f
    | T.Mismatch_record_type (f, r) ->
      S.error ~loc st mismatch_record_type (f, r)
    (* Application of a variable *)
    | T.Var_application v ->
      S.error ~loc st var_application v
    | T.Ty_var_application v ->
      S.error ~loc st ty_var_application v
    (* Wrong type *)
    | T.Type_mismatch (t, expected) ->
      S.error ~loc st type_mismatch (t, expected)
    | T.Quantified_var_inference ->
      S.error ~loc st quant_var_inference ()
    | T.Unhandled_builtin b ->
      S.error ~loc st unhandled_builtin b
    | T.Cannot_tag_tag ->
      S.error ~loc st cannot_tag_tag ()
    | T.Cannot_tag_ttype ->
      S.error ~loc st cannot_tag_ttype ()
    | T.Cannot_find (id, msg) ->
      let lit_hint =
        match S.input_lang st with
        | Some Smtlib2 _ -> true
        | _ -> false
      in
      S.error ~loc st unbound_identifier (id, msg, lit_hint)
    | T.Forbidden_quantifier ->
      S.error ~loc st forbidden_quant ()
    | T.Type_var_in_type_constructor ->
      S.error ~loc st type_var_in_type_cstr ()
    | T.Missing_destructor id ->
      S.error ~loc st missing_destructor id
    | T.Higher_order_application ->
      S.error ~loc st higher_order_app ()
    | T.Higher_order_type ->
      S.error ~loc st higher_order_type ()
    | T.Higher_order_env_in_tff_typechecker ->
      S.error ~loc st higher_order_env_in_tff_typer ()
    | T.Polymorphic_function_argument ->
      S.error ~loc st poly_arg ()
    | T.Non_prenex_polymorphism ty ->
      S.error ~loc st non_prenex_polymorphism ty
    | T.Inference_forbidden (_, w_src, inferred_ty) ->
      S.error ~loc st inference_forbidden (env, w_src, inferred_ty)
    | T.Inference_conflict (_, w_src, inferred_ty, allowed_tys) ->
      S.error ~loc st inference_conflict (env, w_src, inferred_ty, allowed_tys)
    | T.Inference_scope_escape (_, w_src, escaping_var, var_reason) ->
      S.error ~loc st inference_scope_escape (env, w_src, escaping_var, var_reason)
    | T.Unbound_type_wildcards (tys, _) ->
      S.error ~loc st unbound_type_wildcards (env, tys)
    | T.Unhandled_ast ->
      S.error ~loc st unhandled_ast (env, fragment)
    (* Tptp Arithmetic errors *)
    | Tptp_Arith.Expected_arith_type ty ->
      S.error ~loc st expected_arith_type (ty, "")
    | Tptp_Arith.Cannot_apply_to ty ->
      S.error ~loc st expected_specific_arith_type ty
    (* Smtlib Array errors *)
    | Smtlib2_Arrays.Forbidden msg ->
      S.error ~loc st forbidden_array_sort msg
    (* Smtlib Arithmetic errors *)
    | Smtlib2_Ints.Forbidden msg
    | Smtlib2_Reals.Forbidden msg
    | Smtlib2_Reals_Ints.Forbidden msg ->
      S.error ~loc st non_linear_expression msg
    | Smtlib2_Reals_Ints.Expected_arith_type ty ->
      S.error ~loc st expected_arith_type
        (ty, "The stmlib Reals_Ints theory requires an arithmetic type in order to \
              correctly desugar the expression.")
    (* Smtlib Bitvector errors *)
    | Smtlib2_Bitv.Invalid_bin_char c
    | Smtlib2_Float.Invalid_bin_char c ->
      S.error ~loc st invalid_bin_bitvector_char c
    | Smtlib2_Bitv.Invalid_hex_char c
    | Smtlib2_Float.Invalid_hex_char c ->
      S.error ~loc st invalid_hex_bitvector_char c
    | Smtlib2_Bitv.Invalid_dec_char c
    | Smtlib2_Float.Invalid_dec_char c ->
      S.error ~loc st invalid_dec_bitvector_char c
    (* Smtlib String errors *)
    | Smtlib2_String.Invalid_hexadecimal s ->
      S.error ~loc st invalid_hex_string_char s
    | Smtlib2_String.Invalid_string_char c ->
      S.error ~loc st invalid_string_char c
    | Smtlib2_String.Invalid_escape_sequence (s, i) ->
      S.error ~loc st invalid_string_escape_sequence (s, i)
    (* Uncaught exception during type-checking *)
    | T.Uncaught_exn ((Pipeline.Out_of_time |
                       Pipeline.Out_of_space |
                       Pipeline.Sigint) as exn, bt) ->
      Printexc.raise_with_backtrace exn bt
    | T.Uncaught_exn (exn, bt) ->
      S.error ~loc st Report.Error.uncaught_exn (exn, bt)
    (* Bad tptp kind *)
    | Bad_tptp_kind o ->
      S.error ~loc st bad_tptp_kind o
    (* Missing smtlib logic *)
    | Missing_smtlib_logic ->
      S.error ~loc st missing_smtlib_logic ()
    (* Illegal declarations *)
    | Illegal_decl ->
      S.error ~loc st illegal_decl ()
    (* Push/Pop errors *)
    | Invalid_push_n ->
      S.error ~loc st invalid_push ()
    | Invalid_pop_n ->
      S.error ~loc st invalid_pop ()
    | Pop_with_empty_stack ->
      S.error ~loc st empty_pop ()
    (* Catch-all *)
    | _ ->
      S.error ~loc st unknown_error
        (Obj.Extension_constructor.(name (of_val err)))

  (* Generate typing env from state *)
  (* ************************************************************************ *)

  let extract_tptp_kind = function
    | { Dolmen.Std.Term.term = App (
        { term = Symbol id; _ },
        [{ term = Symbol { name = Simple s; _ }; _ }]); _ }
      when Dolmen.Std.Id.(equal tptp_kind) id -> Some s
    | _ -> None

  let rec tptp_kind_of_attrs = function
    | [] -> None
    | t :: r ->
      begin match extract_tptp_kind t with
        | None -> tptp_kind_of_attrs r
        | (Some _) as res -> res
      end

  let builtins_of_smtlib2_logic v (l : Dolmen_type.Logic.Smtlib2.t) =
    List.fold_left (fun acc th ->
        match (th : Dolmen_type.Logic.Smtlib2.theory) with
        | `Core -> Smtlib2_Core.parse v :: acc
        | `Bitvectors -> Smtlib2_Bitv.parse v :: acc
        | `Floats -> Smtlib2_Float.parse v :: acc
        | `String -> Smtlib2_String.parse v :: acc
        | `Arrays ->
          Smtlib2_Arrays.parse ~arrays:l.features.arrays v :: acc
        | `Ints ->
          Smtlib2_Ints.parse ~arith:l.features.arithmetic v :: acc
        | `Reals ->
          Smtlib2_Reals.parse ~arith:l.features.arithmetic v :: acc
        | `Reals_Ints ->
          Smtlib2_Reals_Ints.parse ~arith:l.features.arithmetic v :: acc
      ) [] l.Dolmen_type.Logic.Smtlib2.theories

  let additional_builtins = ref (fun _ _ -> `Not_found : T.builtin_symbols)

  let typing_env ?(attrs=[]) ~loc warnings (st : S.t) =
    let file = S.input_file_loc st in
    let additional_builtins env args = !additional_builtins env args in

    (* Match the language to determine bultins and other options *)
    match (S.input_lang st : Logic.language option) with
    | None -> assert false

    (* Dimacs & iCNF
       - these infer the declarations of their constants
         (we could declare them when the number of clauses and variables
         is declared in the file, but it's easier this way).
       - there are no explicit declaration or definitions, hence no builtins *)
    | Some Dimacs | Some ICNF ->
      let poly = T.Flexible in
      let var_infer = T.{
          infer_type_vars = false;
          infer_term_vars = No_inference;
        } in
      let sym_infer = T.{
          infer_type_csts = false;
          infer_term_csts = Wildcard (Any_base {
              allowed = [Dolmen.Std.Expr.Ty.prop];
              preferred = Dolmen.Std.Expr.Ty.prop;
            });
        } in
      let builtins = Dimacs.parse in
      T.empty_env ~order:First_order
        ~st:(S.ty_state st).typer
        ~var_infer ~sym_infer ~poly
        ~warnings ~file builtins

    (* Alt-Ergo format
    *)
    | Some Alt_ergo ->
      let poly = T.Flexible in
      let var_infer = T.{
          infer_type_vars = true;
          infer_term_vars = No_inference;
        } in
      let sym_infer = T.{
          infer_type_csts = false;
          infer_term_csts = No_inference;
        } in
      let builtins = Dolmen_type.Base.merge [
          Decl.parse;
          Subst.parse;
          additional_builtins;
          Ae_Core.parse;
          Ae_Arith.parse;
          Ae_Bitv.parse;
        ] in
      T.empty_env ~order:First_order
        ~st:(S.ty_state st).typer
        ~var_infer ~sym_infer ~poly
        ~warnings ~file builtins

    (* Zipperposition Format
       - no inference of constants
       - only the base builtin
    *)
    | Some Zf ->
      let poly = T.Flexible in
      let var_infer = T.{
          infer_type_vars = true;
          infer_term_vars = Wildcard Any_in_scope;
        } in
      let sym_infer = T.{
          infer_type_csts = false;
          infer_term_csts = No_inference;
        } in
      let builtins = Dolmen_type.Base.merge [
          Decl.parse;
          Subst.parse;
          additional_builtins;
          Zf_Core.parse;
          Zf_arith.parse
        ] in
      T.empty_env ~order:Higher_order
        ~st:(S.ty_state st).typer
        ~var_infer ~sym_infer ~poly
        ~warnings ~file builtins

    (* TPTP
       - tptp has inference of constants
       - 2 base theories (Core and Arith) + the builtin Decl and Subst
         for explicit declaration and definitions
    *)
    | Some Tptp v ->
      let poly = T.Explicit in
      begin match tptp_kind_of_attrs attrs with
        | Some "thf" ->
          let var_infer = T.{
              infer_type_vars = true;
              infer_term_vars = No_inference;
            } in
          let sym_infer = T.{
              infer_type_csts = false;
              infer_term_csts = No_inference;
            } in
          let builtins = Dolmen_type.Base.merge [
              Decl.parse;
              Subst.parse;
              additional_builtins;
              Tptp_Core_Ho.parse v;
              Tptp_Arith.parse v;
            ] in
          T.empty_env ~order:Higher_order
            ~st:(S.ty_state st).typer
            ~var_infer ~sym_infer ~poly
            ~warnings ~file builtins
        | Some ("tff" | "tpi" | "fof" | "cnf") ->
          let var_infer = T.{
              infer_type_vars = true;
              infer_term_vars = Wildcard (Any_base {
                  allowed = [Dolmen.Std.Expr.Ty.base];
                  preferred = Dolmen.Std.Expr.Ty.base;
                });
            } in
          let sym_infer = T.{
              infer_type_csts = true;
              infer_term_csts = Wildcard (Arrow {
                  arg_shape = Any_base {
                      allowed = [Dolmen.Std.Expr.Ty.base];
                      preferred = Dolmen.Std.Expr.Ty.base;
                    };
                  ret_shape = Any_base {
                      allowed = [
                        Dolmen.Std.Expr.Ty.base;
                        Dolmen.Std.Expr.Ty.prop;
                      ];
                      preferred = Dolmen.Std.Expr.Ty.base;
                    };
                });
            } in
          let builtins = Dolmen_type.Base.merge [
              Decl.parse;
              Subst.parse;
              additional_builtins;
              Tptp_Core.parse v;
              Tptp_Arith.parse v;
            ] in
          T.empty_env ~order:First_order
            ~st:(S.ty_state st).typer
            ~var_infer ~sym_infer ~poly
            ~warnings ~file builtins
        | bad_kind ->
          let builtins = Dolmen_type.Base.noop in
          let env =
            T.empty_env
              ~st:(S.ty_state st).typer
              ~poly ~warnings ~file builtins
          in
          T._error env (Located loc) (Bad_tptp_kind bad_kind)
      end

    (* SMTLib v2
       - no inference
       - see the dedicated function for the builtins
       - restrictions come from the logic declaration
       - shadowing is forbidden
    *)
    | Some Smtlib2 v ->
      let poly = T.Implicit in
      let var_infer = T.{
          infer_type_vars = true;
          infer_term_vars = No_inference;
        } in
      let sym_infer = T.{
          infer_type_csts = false;
          infer_term_csts = No_inference;
        } in
      begin match (S.ty_state st).logic with
        | Auto ->
          let builtins = Dolmen_type.Base.noop in
          let env =
            T.empty_env ~order:First_order
              ~st:(S.ty_state st).typer
              ~var_infer ~sym_infer ~poly
              ~warnings ~file builtins
          in
          T._error env (Located loc) Missing_smtlib_logic
        | Smtlib2 logic ->
          let builtins = Dolmen_type.Base.merge (
              Decl.parse :: Subst.parse :: additional_builtins ::
              builtins_of_smtlib2_logic v logic
            ) in
          let quants = logic.features.quantifiers in
          T.empty_env ~order:First_order
            ~st:(S.ty_state st).typer
            ~var_infer ~sym_infer ~poly ~quants
            ~warnings ~file builtins
      end

  let typing_wrap ?attrs ?(loc=Dolmen.Std.Loc.no_loc) st ~f =
    let st = ref st in
    let report warn = st := report_warning !st warn in
    match f (typing_env ?attrs ~loc report !st) with
    | res -> !st, res
    | exception T.Typing_error err ->
      let st = report_error !st err in
      raise (S.Error st)


  (* Push&Pop *)
  (* ************************************************************************ *)

  let reset st ?loc:_ () =
    S.set_ty_state st (new_state ())

  let reset_assertions st ?loc:_ () =
    let state = S.ty_state st in
    S.set_ty_state st {
      logic = state.logic;
      logic_loc = state.logic_loc;
      typer = T.new_state ();
      stack = [];
    }


  let rec push st ?(loc=Dolmen.Std.Loc.no_loc) = function
    | 0 -> st
    | i ->
      if i < 0 then
        fst @@ typing_wrap ~loc st ~f:(fun env ->
            T._error env (Located loc) Invalid_push_n
          )
      else begin
        let t = S.ty_state st in
        let st' = T.copy_state t.typer in
        let t' = { t with stack = st' :: t.stack; } in
        let st' = S.set_ty_state st t' in
        push st' (i - 1)
      end

  let rec pop st ?(loc=Dolmen.Std.Loc.no_loc) = function
    | 0 -> st
    | i ->
      if i < 0 then
        fst @@ typing_wrap ~loc st ~f:(fun env ->
            T._error env (Located loc) Invalid_pop_n
          )
      else begin
        let t = S.ty_state st in
        match t.stack with
        | [] ->
          fst @@ typing_wrap ~loc st ~f:(fun env ->
              T._error env (Located loc) Pop_with_empty_stack
            )
        | ty :: r ->
          let t' = { t with typer = ty; stack = r; } in
          let st' = S.set_ty_state st t' in
          pop st' (i - 1)
      end


  (* Setting the logic *)
  (* ************************************************************************ *)

  let set_logic_aux ~loc st new_logic =
    let ty_st = S.ty_state st in
    let st =
      match ty_st.logic with
      | Auto -> st
      | Smtlib2 _ -> S.warn ~loc st logic_reset ty_st.logic_loc
    in
    S.set_ty_state st {
      ty_st with
      logic = new_logic;
      logic_loc = Dolmen.Std.Loc.full_loc loc;
    }

  let set_logic (st : S.t) ?(loc=Dolmen.Std.Loc.no_loc) s =
    let file = S.input_file_loc st in
    let loc : Dolmen.Std.Loc.full = { file; loc; } in
    match (S.input_lang st : Logic.language option) with
    | Some ICNF -> st
    | Some Dimacs -> st
    | Some Smtlib2 _ ->
      let st, l =
        match Dolmen_type.Logic.Smtlib2.parse s with
        | Some l -> st, l
        | None ->
          let st = S.warn ~loc st unknown_logic s in
          st, Dolmen_type.Logic.Smtlib2.all
      in
      set_logic_aux ~loc st (Smtlib2 l)
    | _ ->
      S.warn ~loc st set_logic_not_supported ()


  (* Declarations *)
  (* ************************************************************************ *)

  let allow_function_decl (st : S.t) =
    match (S.ty_state st).logic with
    | Smtlib2 logic -> logic.features.free_functions
    | Auto -> true

  let allow_data_type_decl (st : S.t) =
    match (S.ty_state st).logic with
    | Smtlib2 logic -> logic.features.datatypes
    | Auto -> true

  let allow_abstract_type_decl (st : S.t) =
    match (S.ty_state st).logic with
    | Smtlib2 logic -> logic.features.free_sorts
    | Auto -> true

  let check_decl st env d = function
    | `Type_decl (c : Dolmen.Std.Expr.ty_cst) ->
      begin match Dolmen.Std.Expr.Ty.definition c with
        | None | Some Abstract ->
          if not (allow_abstract_type_decl st) then
            T._error env (Decl d) Illegal_decl
        | Some Adt _ ->
          if not (allow_data_type_decl st) then
            T._error env (Decl d) Illegal_decl
      end
    | `Term_decl (c : Dolmen.Std.Expr.term_cst) ->
      let is_function =
        let vars, args, _ = Dolmen.Std.Expr.Ty.poly_sig c.id_ty in
        vars <> [] || args <> []
      in
      if is_function && not (allow_function_decl st) then
        T._error env (Decl d) Illegal_decl

  let check_decls st env l decls =
    List.iter2 (check_decl st env) l decls

  let decls (st : S.t) ?loc ?attrs d =
    typing_wrap ?attrs ?loc st ~f:(fun env ->
        let decls = T.decls env ?attrs d in
        let () = check_decls st env d.contents decls in
        decls
      )


  (* Definitions *)
  (* ************************************************************************ *)

  let defs st ?loc ?attrs d =
    typing_wrap ?attrs ?loc st ~f:(fun env ->
        let l = T.defs env ?attrs d in
        let l = List.map (function
            | `Type_def (id, c, vars, body) ->
              (* are recursive defs interesting to expand ? *)
              let () =
                if not d.recursive then Dolmen.Std.Expr.Ty.alias_to c vars body
              in
              let () = Decl.add_definition env id (`Ty c) in
              `Type_def (id, c, vars, body)
            | `Term_def (id, f, vars, args, body) ->
              let () = Decl.add_definition env id (`Term f) in
              `Term_def (id, f, vars, args, body)
          ) l
        in
        l
      )

  (* Wrappers around the Type-checking module *)
  (* ************************************************************************ *)

  let typecheck = S.typecheck

  let terms st ?loc ?attrs = function
    | [] -> st, []
    | l ->
      typing_wrap ?attrs ?loc st ~f:(fun env ->
          List.map (T.parse_term env) l
        )

  let formula st ?loc ?attrs ~goal:_ (t : Dolmen.Std.Term.t) =
    typing_wrap ?attrs ?loc st ~f:(fun env ->
        T.parse env t
      )

  let formulas st ?loc ?attrs = function
    | [] -> st, []
    | l ->
      typing_wrap ?attrs ?loc st ~f:(fun env ->
          List.map (T.parse env) l
        )

end


(* Pipes functor *)
(* ************************************************************************ *)

module type Pipe_arg = Typer_intf.Pipe_arg
module type Pipe_res = Typer_intf.Pipe_res

module Pipe
    (Expr : Expr_intf.S)
    (Print : Expr_intf.Print
     with type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
    (State : State_intf.Typer_pipe)
    (Typer : Typer_intf.Pipe_arg
     with type state := State.t
      and type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
= struct

  module S = Dolmen.Std.Statement

  (* Types used in Pipes *)
  (* ************************************************************************ *)

  (* Used for representing typed statements *)
  type +'a stmt = {
    id : Dolmen.Std.Id.t;
    loc : Dolmen.Std.Loc.t;
    contents  : 'a;
  }

  type def = [
    | `Type_def of Dolmen.Std.Id.t * Expr.ty_cst * Expr.ty_var list * Expr.ty
    | `Term_def of Dolmen.Std.Id.t * Expr.term_cst * Expr.ty_var list * Expr.term_var list * Expr.term
  ]

  type defs = [
    | `Defs of def list
  ]

  type decl = [
    | `Type_decl of Expr.ty_cst
    | `Term_decl of Expr.term_cst
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
    | `Plain of Dolmen.Std.Statement.term
  ]

  type set_info = [
    | `Set_logic of string
    | `Set_info of Dolmen.Std.Statement.term
    | `Set_option of Dolmen.Std.Statement.term
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

  let print_def fmt = function
    | `Type_def (id, c, vars, body) ->
      Format.fprintf fmt "@[<hov 2>type-def:@ %a: %a(%a) ->@ %a@]"
        Dolmen.Std.Id.print id Print.ty_cst c
        (Format.pp_print_list Print.ty_var) vars Print.ty body
    | `Term_def (id, c, vars, args, body) ->
      Format.fprintf fmt
        "@[<hv 2>term-def{%a}:@ @[<hv>%a@] =@ @[<hov 2>fun (%a;@ %a) ->@ %a@]@]"
        Dolmen.Std.Id.print id Print.term_cst c
        (Format.pp_print_list Print.ty_var) vars
        (Format.pp_print_list Print.term_var) args
        Print.term body

  let print_decl fmt = function
    | `Type_decl c ->
      Format.fprintf fmt "@[<hov 2>type-decl:@ %a@]" Print.ty_cst c
    | `Term_decl c ->
      Format.fprintf fmt "@<hov 2>term-decl:@ %a@]" Print.term_cst c

  let print_typechecked fmt t =
    match (t : typechecked) with
    | `Defs l ->
      Format.fprintf fmt "@[<v 2>defs:@ %a@]"
        (Format.pp_print_list print_def) l
    | `Decls l ->
      Format.fprintf fmt "@[<v 2>decls:@ %a@]"
        (Format.pp_print_list print_decl) l
    | `Hyp f ->
      Format.fprintf fmt "@[<hov 2>hyp:@ %a@]" Print.formula f
    | `Goal f ->
      Format.fprintf fmt "@[<hov 2>goal:@ %a@]" Print.formula f
    | `Clause l ->
      Format.fprintf fmt "@[<v 2>clause:@ %a@]"
        (Format.pp_print_list Print.formula) l
    | `Solve l ->
      Format.fprintf fmt "@[<hov 2>solve-assuming: %a@]"
        (Format.pp_print_list Print.formula) l
    | _ ->
      Format.fprintf fmt "TODO"

  let print fmt ({ id; loc = _; contents; } : typechecked stmt) =
    Format.fprintf fmt "%a:@ %a"
      Dolmen.Std.Id.print id print_typechecked contents

  (* Typechecking *)
  (* ************************************************************************ *)

  let stmt_id ref_name =
    let counter = ref 0 in
    (fun c ->
       match c.Dolmen.Std.Statement.id with
       | None ->
         let () = incr counter in
         let name = Format.sprintf "%s_%d" ref_name !counter in
         Dolmen.Std.Id.mk Dolmen.Std.Id.decl name
       | Some id -> id)

  let def_id   = stmt_id "def"
  let decl_id  = stmt_id "decl"
  let hyp_id   = stmt_id "hyp"
  let goal_id  = stmt_id "goal"
  let prove_id = stmt_id "prove"
  let other_id = stmt_id "other"

  let fv_list l =
    let l' = List.map Dolmen.Std.Term.fv l in
    List.sort_uniq Dolmen.Std.Id.compare (List.flatten l')

  let fp_list (ty : Dolmen.Std.Term.t) =
    match ty.term with
    | Binder (Arrow, params_tys, ret_ty) ->
      fv_list (ret_ty :: params_tys)
    | _ -> fv_list [ty]

  let quantify ~loc var_ty vars f =
    let vars = List.map (fun v ->
        let c = Dolmen.Std.Term.const ~loc v in
        match var_ty v with
        | None -> c
        | Some ty -> Dolmen.Std.Term.colon c ty
      ) vars in
    Dolmen.Std.Term.forall ~loc vars f

  let normalize st c =
    match c with
    (* Clauses without free variables can be typechecked as is
       without worry, but if there are free variables, these must
       be quantified or else the typchecker will raise an error. *)
    | { S.descr = S.Clause l; _ } ->
      begin match fv_list l with
        | [] -> c

        | free_vars ->
          let loc = c.S.loc in
          let f = match l with
            | [] -> assert false
            | [p] -> p
            | _ -> Dolmen.Std.Term.apply ~loc (Dolmen.Std.Term.or_t ~loc ()) l
          in
          let f = quantify ~loc (fun _ -> None) free_vars f in
          { c with descr = S.Antecedent f; }
      end
    (* Axioms, goals and definitions in alt-ergo have their type variables
       implicitly quantified. *)
    | { S.descr = S.Defs { contents = l; recursive; } ; _ } ->
      let l' = List.map (function ({ S.ty; body; _ } as d) ->
        match fp_list ty with
        | [] -> d
        | l ->
          let vars = List.map (fun v -> Dolmen.Std.Term.const v) l in
          let ty = Dolmen.Std.Term.pi vars ty in
          let body =
            Dolmen.Std.Term.lambda (List.map (fun v ->
                Dolmen.Std.Term.colon v (Dolmen.Std.Term.tType ())
              ) vars) body
          in
          { d with ty; body; }
        ) l
      in
      { c with descr = S.Defs { recursive; contents = l'; }; }
    | { S.descr = S.Antecedent t; _ }
      when State.input_lang st = Some Logic.Alt_ergo ->
      begin match fv_list [t] with
        | [] -> c
        | free_vars ->
          let loc = c.S.loc in
          let var_ttype _ = Some (Dolmen.Std.Term.tType ~loc ()) in
          let f = quantify ~loc var_ttype free_vars t in
          { c with descr = S.Antecedent f; }
      end
    | { S.descr = S.Consequent t; _ }
      when State.input_lang st = Some Logic.Alt_ergo ->
      begin match fv_list [t] with
        | [] -> c
        | free_vars ->
          let loc = c.S.loc in
          let var_ttype _ = Some (Dolmen.Std.Term.tType ~loc ()) in
          let f = quantify ~loc var_ttype free_vars t in
          { c with descr = S.Consequent f; }
      end


    (* catch all *)
    | _ -> c

  let typecheck st c =
    let res =
      if not (Typer.typecheck st) then
        st, `Done ()
      else match normalize st c with

        (* Pack and includes.
           These should have been filtered out before this point.
           TODO: emit some kind of warning ? *)
        | { S.descr = S.Pack _; _ } -> st, `Done ()
        | { S.descr = S.Include _; _ } -> st, `Done ()

        (* State&Assertion stack management *)
        | { S.descr = S.Reset; _ } ->
          let st = Typer.reset st ~loc:c.S.loc () in
          st, `Continue (simple (other_id c) c.S.loc `Reset)
        | { S.descr = S.Pop i; _ } ->
          let st = Typer.pop st ~loc:c.S.loc i in
          st, `Continue (simple (other_id c) c.S.loc (`Pop i))
        | { S.descr = S.Push i; _ } ->
          let st = Typer.push st ~loc:c.S.loc i in
          st, `Continue (simple (other_id c) c.S.loc (`Push i))
        | { S.descr = S.Reset_assertions; _ } ->
          let st = Typer.reset_assertions st ~loc:c.S.loc () in
          st, `Continue (simple (other_id c) c.S.loc `Reset_assertions)

        (* Plain statements
           TODO: allow the `plain` function to return a meaningful value *)
        | { S.descr = S.Plain t; _ } ->
          st, `Continue (simple (other_id c) c.S.loc (`Plain t))

        (* Hypotheses and goal statements *)
        | { S.descr = S.Prove l; _ } ->
          let st, l = Typer.formulas st ~loc:c.S.loc ~attrs:c.S.attrs l in
          st, `Continue (simple (prove_id c) c.S.loc (`Solve l))

        (* Hypotheses & Goals *)
        | { S.descr = S.Clause l; _ } ->
          let st, res = Typer.formulas st ~loc:c.S.loc ~attrs:c.S.attrs l in
          let stmt : typechecked stmt = simple (hyp_id c) c.S.loc (`Clause res) in
          st, `Continue stmt
        | { S.descr = S.Antecedent t; _ } ->
          let st, ret = Typer.formula st ~loc:c.S.loc ~attrs:c.S.attrs ~goal:false t in
          let stmt : typechecked stmt = simple (hyp_id c) c.S.loc (`Hyp ret) in
          st, `Continue stmt
        | { S.descr = S.Consequent t; _ } ->
          let st, ret = Typer.formula st ~loc:c.S.loc ~attrs:c.S.attrs ~goal:true t in
          let stmt : typechecked stmt = simple (goal_id c) c.S.loc (`Goal ret) in
          st, `Continue stmt

        (* Other set_logics should check whether corresponding plugins are activated ? *)
        | { S.descr = S.Set_logic s; _ } ->
          let st = Typer.set_logic st ~loc:c.S.loc s in
          st, `Continue (simple (other_id c) c.S.loc (`Set_logic s))

        (* Set/Get info *)
        | { S.descr = S.Get_info s; _ } ->
          st, `Continue (simple (other_id c) c.S.loc (`Get_info s))
        | { S.descr = S.Set_info t; _ } ->
          st, `Continue (simple (other_id c) c.S.loc (`Set_info t))

        (* Set/Get options *)
        | { S.descr = S.Get_option s; _ } ->
          st, `Continue (simple (other_id c) c.S.loc (`Get_option s))
        | { S.descr = S.Set_option t; _ } ->
          st, `Continue (simple (other_id c) c.S.loc (`Set_option t))

        (* Declarations and definitions *)
        | { S.descr = S.Defs d; _ } ->
          let st, l = Typer.defs st ~loc:c.S.loc ~attrs:c.S.attrs d in
          let res : typechecked stmt = simple (def_id c) c.S.loc (`Defs l) in
          st, `Continue (res)
        | { S.descr = S.Decls l; _ } ->
          let st, l = Typer.decls st ~loc:c.S.loc ~attrs:c.S.attrs l in
          let res : typechecked stmt = simple (decl_id c) c.S.loc (`Decls l) in
          st, `Continue (res)

        (* Smtlib's proof/model instructions *)
        | { S.descr = S.Get_proof; _ } ->
          st, `Continue (simple (other_id c) c.S.loc `Get_proof)
        | { S.descr = S.Get_unsat_core; _ } ->
          st, `Continue (simple (other_id c) c.S.loc `Get_unsat_core)
        | { S.descr = S.Get_unsat_assumptions; _ } ->
          st, `Continue (simple (other_id c) c.S.loc `Get_unsat_assumptions)
        | { S.descr = S.Get_model; _ } ->
          st, `Continue (simple (other_id c) c.S.loc `Get_model)
        | { S.descr = S.Get_value l; _ } ->
          let st, l = Typer.terms st ~loc:c.S.loc ~attrs:c.S.attrs l in
          st, `Continue (simple (other_id c) c.S.loc (`Get_value l))
        | { S.descr = S.Get_assignment; _ } ->
          st, `Continue (simple (other_id c) c.S.loc `Get_assignment)
        (* Assertions *)
        | { S.descr = S.Get_assertions; _ } ->
          st, `Continue (simple (other_id c) c.S.loc `Get_assertions)
        (* Misc *)
        | { S.descr = S.Echo s; _ } ->
          st, `Continue (simple (other_id c) c.S.loc (`Echo s))
        | { S.descr = S.Exit; _ } ->
          st, `Continue (simple (other_id c) c.S.loc `Exit)

    in
    res

end

