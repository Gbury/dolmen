
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Dolmen_type functors instantiation *)
(* ************************************************************************ *)

module T = Dolmen_type.Thf.Make
    (Dolmen.Std.Tag)(Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)

(* AE builtins *)
module Ae_Core =
  Dolmen_type.Core.Ae.Tff(T)(Dolmen.Std.Expr.Tags)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Ae_Arith =
  Dolmen_type.Arith.Ae.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)
module Ae_Arrays =
  Dolmen_type.Arrays.Ae.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Array)
module Ae_Bitv =
  Dolmen_type.Bitv.Ae.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Bitv)
module Ae_Float =
  Dolmen_type.Float.Ae.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term)

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
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Array)
module Smtlib2_Bitv =
  Dolmen_type.Bitv.Smtlib2.Tff(T)
    (Dolmen.Std.Expr.Ty)(Dolmen.Std.Expr.Term.Bitv)
module Smtlib2_Bvconv =
  Dolmen_type.Bitv.Smtlib2.Bvconv(T)
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

let print_var_kind fmt k =
  match (k : T.var_kind) with
  | `Let_bound -> Format.fprintf fmt "let-bound variable"
  | `Quantified -> Format.fprintf fmt "quantified variable"
  | `Function_param -> Format.fprintf fmt "function parameter"
  | `Type_alias_param -> Format.fprintf fmt "type alias parameter"

let print_reason ?(already=false) fmt r =
  let pp_already fmt () =
    if already then Format.fprintf fmt " already"
  in
  match (r : T.reason) with
  | Builtin ->
    Format.fprintf fmt "is%a defined by a builtin theory" pp_already ()
  | Reserved (Strict, reason) ->
    Format.fprintf fmt "is reserved for %a" Format.pp_print_text reason
  | Reserved (Model_completion, reason) ->
    Format.fprintf fmt "is reserved for %a.@ %a"
      Format.pp_print_text reason
      Format.pp_print_text
      "Therefore, the definition of the model corner case would take \
       priority and prevent defining a value for this constant."
  | Bound (file, ast) ->
    Format.fprintf fmt "was%a bound at %a"
      pp_already () Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file ast.loc)
  | Inferred (file, ast) ->
    Format.fprintf fmt "was%a inferred at %a"
      pp_already () Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file ast.loc)
  | Defined (file, d) ->
    Format.fprintf fmt "was%a defined at %a"
      pp_already () Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file d.loc)
  | Declared (file, d) ->
    Format.fprintf fmt "was%a declared at %a"
      pp_already () Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file (decl_loc d))
  | Implicit_in_def (file, d) ->
    Format.fprintf fmt "was%a implicitly introduced in the definition at %a"
      pp_already () Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file d.loc)
  | Implicit_in_decl (file, d) ->
    Format.fprintf fmt "was%a implicitly introduced in the declaration at %a"
      pp_already () Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file (decl_loc d))
  | Implicit_in_term (file, ast) ->
    Format.fprintf fmt "was%a implicitly introduced in the term at %a"
      pp_already () Dolmen.Std.Loc.fmt_pos (Dolmen.Std.Loc.loc file ast.loc)

let print_reason_opt ?already fmt = function
  | Some r -> print_reason ?already fmt r
  | None -> Format.fprintf fmt "was bound at <location missing>"

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
      "Symbol@ %a@ is@ located@ at@ %a"
      (pp_wrap Dolmen.Std.Id.print) symbol
      Dolmen.Std.Loc.fmt_pos loc
  | T.Variable_inference { variable; variable_loc; inferred_ty = _; } ->
    let loc = Dolmen.Std.Loc.full_loc (T.loc env variable_loc) in
    Format.fprintf fmt
      "Variable@ %a@ was@ bound@ at@ %a"
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

let print_reserved fmt = function
  | `Solver reason ->
    Format.fprintf fmt "to be used by solvers for %a"
      Format.pp_print_text reason

(* Hint printers *)
(* ************************************************************************ *)

let fo_hint _ =
  Some (
    Format.dprintf "%a" Format.pp_print_text
      "This statement was parsed as a first-order statement")

let text_hint = function
  | "" -> None
  | msg -> Some (Format.dprintf "%a" Format.pp_print_text msg)

let text_hint2 = function
  | _, "" -> None
  | _, msg -> Some (Format.dprintf "%a" Format.pp_print_text msg)

let poly_hint (c, expected, actual) =
  let vars, params, _ = Dolmen.Std.Expr.(Ty.poly_sig @@ Term.Const.ty c) in
  let n_ty = List.length vars in
  let n_t = List.length params in
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

let bv_expected_nat_lit_hint _ =
  Some (
    Format.dprintf "%a" Format.pp_print_text
      "Some bitvector functions in alt-ergo require their first argument \
       to be a literal natural number.")

(* Typing warnings *)
(* ************************************************************************ *)

let code = Code.typing

let unused_type_variable =
  Report.Warning.mk ~code ~mnemonic:"unused-type-var"
    ~message:(fun fmt (kind, v) ->
        Format.fprintf fmt
          "The following %a is unused: '%a'"
          print_var_kind kind Dolmen.Std.Expr.Print.id v)
    ~name:"Unused bound type variable" ()

let unused_term_variable =
  Report.Warning.mk ~code ~mnemonic:"unused-term-var"
    ~message:(fun fmt (kind, v) ->
        Format.fprintf fmt
          "The following %a is unused: `%a`"
          print_var_kind kind Dolmen.Std.Expr.Print.id v)
    ~name:"Unused bound term variable" ()

let unknown_attribute =
  Report.Warning.mk ~code ~mnemonic:"unknown-attribute"
    ~message:(fun fmt id ->
        Format.fprintf fmt "Unknown attribute (the attribtue was ignored): %a"
          (pp_wrap Dolmen.Std.Id.print) id)
    ~name:"Unknown attribute" ()

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
          "Shadowing: %a %a"
          (pp_wrap Dolmen.Std.Id.print) id
          (print_reason_opt ~already:true) (T.binding_reason old))
    ~name:"Shadowing of identifier" ()

let redundant_pattern =
  Report.Warning.mk ~code ~mnemonic:"redundant-pattern"
    ~message:(fun fmt _pattern ->
        Format.fprintf fmt
          "This pattern is useless (i.e. it is made redundant by earlier patterns)")
    ~name:"Redundant pattern" ()

let bad_arith_expr =
  Report.Warning.mk ~code ~mnemonic:"bad-arith-expr"
    ~message:(fun fmt (config, _) ->
        Format.fprintf fmt
          "This expression does not conform to the specification for %a arithmetic"
          Dolmen_type.Arith.Smtlib2.print_config config)
    ~hints:[text_hint2]
    ~name:"Non-linear expression in linear arithmetic" ()

let array_extension =
  Report.Warning.mk ~code ~mnemonic:"array-extension"
    ~message:(fun fmt c ->
        Format.fprintf fmt
          "The symbol %a is an extension of the array theory and is not \
           defined by the smtlib specification." Dolmen.Std.Id.print c)
    ~name:"Use of Extensions of the Array theory" ()

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

let partial_pattern_match =
  Report.Error.mk ~code ~mnemonic:"partial-pattern-match"
    ~message:(fun fmt missing ->
        let pp_sep fmt () = Format.fprintf fmt "@ " in
        Format.fprintf fmt
          "This pattern matching is not exhaustive.@ \
           Here is an example of a non-matching value:@ @[<v>%a@]"
          (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Term.print) missing
      )
    ~name:"Missing cases in pattern matching" ()

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

let var_in_binding_pos_underspecified =
  Report.Error.mk ~code ~mnemonic:"var-binding-infer"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Cannot infer type for a variable in binding position")
    ~name:"Inference of a variable in binding position's type" ()

let unhandled_builtin =
  Report.Error.mk ~code:Code.bug ~mnemonic:"typer-unhandled-builtin"
    ~message:(fun fmt b ->
        Format.fprintf fmt
          "The following Dolmen builtin is currently not handled during typing@ %a.@ \
           Please report upstream"
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
          "Duplicate declaration of %a, which %a"
          (pp_wrap Dolmen.Std.Id.print) id
          (print_reason_opt ~already:true) (T.binding_reason old))
    ~name:"Multiple declarations of the same symbol" ()

let forbidden_quant =
  Report.Error.mk ~code ~mnemonic:"forbidden-quant"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Quantified expressions are forbidden by the logic.")
    ~name:"Forbidden quantifier" ()

let missing_destructor =
  Report.Error.mk ~code:Code.bug ~mnemonic:"missing-destructor"
    ~message:(fun fmt id ->
        Format.fprintf fmt
          "The destructor %a@ was not provided by the user implementation.@ \
           Please report upstream."
          (pp_wrap Dolmen.Std.Id.print) id)
    ~name:"Missing destructor in implementation" ()

let type_def_rec =
  Report.Error.mk ~code ~mnemonic:"type-def-rec"
    ~message:(fun fmt _ ->
        Format.fprintf fmt
          "Only value definition are allowed in recursive definitions,@ \
           but this recursive definition contains a type definition")
    ~name:"Type definition in recursive value definition" ()

let id_definition_conflict =
  Report.Error.mk ~code ~mnemonic:"id-def-conflict"
    ~message:(fun fmt (id, binding) ->
        match (binding : T.binding) with
        | `Not_found ->
          Format.fprintf fmt
            "Trying to define a model value for a symbol \
             non-declared in the original problem: %a"
            (pp_wrap Dolmen.Std.Id.print) id
        | `Builtin _ ->
          Format.fprintf fmt
            "Trying to define a model value for a symbol \
             with a builtin interpretation: %a"
            (pp_wrap Dolmen.Std.Id.print) id
        | _ ->
          Format.fprintf fmt
            "Trying to define a model value for symbol %a,@ \
             but the symbol %a"
            (pp_wrap Dolmen.Std.Id.print) id
            (print_reason_opt ~already:true) (T.binding_reason binding))
    ~name:"Conflicting id definition" ()

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
           @[<hov>Variable %a@ %a.@]\
           @]"
          print_wildcard_origin w_src
          (print_wildcard_path env) w_src
          (pp_wrap Dolmen.Std.Expr.Ty.Var.print) escaping_var
          (print_wildcard_loc env) w_src
          (pp_wrap Dolmen.Std.Expr.Ty.Var.print) escaping_var
          (print_reason_opt ~already:false) var_reason)
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

let incoherent_type_redefinition =
  Report.Error.mk ~code ~mnemonic:"incoherent-type-def"
    ~message:(fun fmt (id, cst, reason, n) ->
        Format.fprintf fmt
          "Incoherent redefinition for type constructor %a.@ \
           The new definition has arity %d,@ \
           but the declaration had arity %d.@ \
           %a %a"
          (pp_wrap Dolmen.Std.Id.print) id
          n (Dolmen.Std.Expr.Ty.Const.arity cst)
          (pp_wrap Dolmen.Std.Id.print) id
          (print_reason ~already:false) reason
      )
    ~name:"Incoherent arity for type definition" ()

let incoherent_term_redefinition =
  Report.Error.mk ~code ~mnemonic:"incoherent-term-def"
    ~message:(fun fmt (id, cst, reason, ty) ->
        Format.fprintf fmt
          "@[<v>Incoherent redefinition for term constant %a.@ \
           The new definition has type:@   @[<hov>%a@]@ \
           but the declaration had type:@   @[<hov>%a@]@ \
           %a %a@]"
          (pp_wrap Dolmen.Std.Id.print) id
          Dolmen.Std.Expr.Ty.print ty
          Dolmen.Std.Expr.Ty.print (Dolmen.Std.Expr.Term.Const.ty cst)
          (pp_wrap Dolmen.Std.Id.print) id
          (print_reason ~already:false) reason
      )
    ~name:"Incoherent type for term definition" ()

let unhandled_ast : (T.env * Dolmen_std.Term.t T.fragment) Report.Error.t =
  Report.Error.mk ~code ~mnemonic:"unhandled-ast"
    ~message:(fun fmt (env, fragment) ->
        Format.fprintf fmt
          "The typechecker did not know what to do with the following term.@ \
           Please report upstream.@\n%a"
          print_fragment (env, fragment))
    ~name:"Unhandled AST fragment" ()

let bad_farray_arity =
  Report.Error.mk ~code ~mnemonic:"bad-farray-arity"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Functional array types in Alt-Ergo expect either one or two type \
                            parameters.")
    ~name:"Bad functional array arity" ()

let expected_arith_type =
  Report.Error.mk ~code ~mnemonic:"arith-type-expected"
    ~message:(fun fmt (ty, _) ->
        Format.fprintf fmt "Arithmetic type expected but got@ %a.@"
          (pp_wrap Dolmen.Std.Expr.Ty.print) ty)
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

let forbidden_arith_expr =
  Report.Error.mk ~code ~mnemonic:"forbidden-arith"
    ~message:(fun fmt (config, _msg) ->
        Format.fprintf fmt "Forbidden expression in %a arithmetic"
          Dolmen_type.Arith.Smtlib2.print_config config)
    ~hints:[text_hint2]
    ~name:"Forbidden arithmetic expression" ()

let bitvector_app_expected_nat_lit =
  Report.Error.mk ~code ~mnemonic:"bitvector-app-expected-nat-lit"
    ~message:(fun fmt t ->
        Format.fprintf fmt "Expected a natural number literal as an argument, \
                            but instead got the following untyped term:@ %a"
          Dolmen_std.Term.print t)
    ~hints:[bv_expected_nat_lit_hint]
    ~name:"Bad bitvector application argument" ()

let non_positive_bitv_size =
  Report.Error.mk ~code ~mnemonic:"bitvector-non-positive-size"
    ~message:(fun fmt i ->
        Format.fprintf fmt "%a: %d"
          Format.pp_print_text
          "Bitvector sizes must be positive (i.e. > 0), \
           which is not the case for the following size" i)
    ~name:"Non positive bitvector size" ()

let invalid_smt2_bitv_extract =
  Report.Error.mk ~code ~mnemonic:"bitvector-smt2-extract"
    ~message:(fun fmt (i, j, m) ->
        if not (j <= i) then
          Format.fprintf fmt "Indexes i=%d and j=%d@ %a"
            i j Format.pp_print_text
            "in (_ extract i j) must be so that j <= i"
        else if not (i < m) then
          Format.fprintf fmt
            "Out of bound extract: (_ extract %d %d) %a >= %d,@ %a %d"
            i j Format.pp_print_text
            "must be applied to a bitvector of a size" i
            Format.pp_print_text
            "but is here applied to a bitvector of size" m
        else
          Format.fprintf fmt
            "Invalid bitvector extract (please report upstream, ^^)"
      )
    ~name:"Invalid smtlib2 bitvector extract" ()

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

let illegal_def =
  Report.Error.mk ~code ~mnemonic:"illegal-def"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Illegal definition.")
    ~name:"Illegal definition" ()

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

let reserved =
  Report.Error.mk ~code ~mnemonic:"reserved"
    ~message:(fun fmt (id, reserved) ->
        Format.fprintf fmt
          "@[<hov>Reserved: %a is reserved %a."
          (pp_wrap Dolmen.Std.Id.print) id
          print_reserved reserved)
    ~name:"Shadowing of reserved identifier" ()

let incorrect_sexpression =
  Report.Error.mk ~code ~mnemonic:"incorrect-sexpression"
    ~message:(fun fmt msg ->
        Format.fprintf fmt "Incorrect s-expression: %t" msg
      )
    ~name:"Incorrect S-expression" ()

let non_closed_named_term =
  Report.Error.mk ~code ~mnemonic:"non-closed-named-term"
    ~message:(fun fmt (ty_vars, t_vars) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "%a:@ %a%a%a"
          Format.pp_print_text
          "Named terms must be closed, but the following variables \
           are free"
          (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Ty.Var.print) ty_vars
          (match ty_vars with
           | [] -> (fun _ _ -> ())
           | _ :: _ -> pp_sep
          ) ()
          (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Term.Var.print) t_vars
      )
    ~name:"Non-closed Named Term" ()

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

let typer_state { typer; _ } = typer


(* Make functor *)
(* ************************************************************************ *)

type 'a file = 'a State.file

module type Typer_Full = Typer_intf.Typer_Full

module Typer(State : State.S) = struct

  (* Type aliases *)
  (* ************************************************************************ *)

  type state = State.t
  type nonrec ty_state = ty_state
  type env = T.env
  type 'a fragment = 'a T.fragment
  type error = T.error
  type warning = T.warning
  type builtin_symbols = T.builtin_symbols

  type lang = [
    | `Logic of Logic.language
    | `Response of Response.language
  ]

  (* Extensions builtins *)
  (* ************************************************************************ *)

  module Ext = struct

    type t = {
      name : string;
      builtins : lang -> T.builtin_symbols;
    }

    let all = ref []
    let list () = !all
    let name { name; _ } = name
    let builtins { builtins; _ } = builtins

    let create ~name ~builtins =
      let t = { name; builtins; } in
      all := t :: !all;
      t

    let bv2nat =
      create ~name:"bvconv"
        ~builtins:(function
            | `Logic Logic.Smtlib2 version ->
              Smtlib2_Bvconv.parse (`Script version)
            | _ -> Dolmen_type.Base.noop)

  end

  (* State setup *)
  (* ************************************************************************ *)

  let pipe = "Typer"
  let ty_state : ty_state State.key =
    State.create_key ~pipe "ty_state"
  let check_model : bool State.key =
    State.create_key ~pipe:"Model" "check_model"
  let smtlib2_forced_logic : string option State.key =
    State.create_key ~pipe "smtlib2_forced_logic"
  let extension_builtins : Ext.t list State.key =
    State.create_key ~pipe "extensions_builtins"
  let additional_builtins : (state -> lang -> T.builtin_symbols) State.key =
    State.create_key ~pipe "additional_builtins"

  let init
      ?ty_state:(ty_state_value=new_state ())
      ?smtlib2_forced_logic:(smtlib2_forced_logic_value=None)
      ?extension_builtins:(extension_builtins_value=[])
      ?additional_builtins:(additional_builtins_value=fun _ _ _ _ -> `Not_found)
      st =
    st
    |> State.set ty_state ty_state_value
    |> State.set smtlib2_forced_logic smtlib2_forced_logic_value
    |> State.set extension_builtins extension_builtins_value
    |> State.set additional_builtins additional_builtins_value


  (* Input helpers *)
  (* ************************************************************************ *)

  type input = [
    | `Logic of Logic.language file
    | `Response of Response.language file
  ]

  let warn ~input ~loc st warn payload =
    match (input : input) with
    | `Logic file -> State.warn ~file ~loc st warn payload
    | `Response file -> State.warn ~file ~loc st warn payload

  let error ~input ~loc st err payload =
    match (input : input) with
    | `Logic file -> State.error ~file ~loc st err payload
    | `Response file -> State.error ~file ~loc st err payload

  let file_loc_of_input (input : input) =
    match input with
    | `Logic f -> f.loc
    | `Response f -> f.loc

  let lang_of_input (input : input) : [ lang | `Missing ]=
    match input with
    | `Logic f ->
      begin match f.lang with
        | None -> `Missing
        | Some l -> `Logic l
      end
    | `Response f ->
      begin match f.lang with
        | None -> `Missing
        | Some l -> `Response l
      end

  (* New warnings & errors *)
  (* ************************************************************************ *)

  type _ T.err +=
    | Bad_tptp_kind : string option -> Dolmen.Std.Loc.t T.err
    | Missing_smtlib_logic : Dolmen.Std.Loc.t T.err
    | Illegal_decl : Dolmen.Std.Statement.decl T.err
    | Illegal_def : Dolmen.Std.Loc.t T.err
    | Invalid_push_n : Dolmen.Std.Loc.t T.err
    | Invalid_pop_n : Dolmen.Std.Loc.t T.err
    | Pop_with_empty_stack : Dolmen.Std.Loc.t T.err

  (* Report type warnings *)
  (* ************************************************************************ *)

  let var_can_be_unused (v : _ Dolmen.Std.Expr.id) =
    match v.path with
    | Local { name; } when String.length name >= 1 && name.[0] = '_' -> true
    | _ -> false

  let smtlib2_6_shadow_rules (input : input) =
    match input with
    | `Logic { lang = Some Smtlib2 (`Latest | `V2_6 | `Poly); _ }
    | `Response { lang = Some Smtlib2 (`Latest | `V2_6); _ }
      -> true
    | _ -> false

  let typing_logic input =
    match (input : input) with
    | `Logic _ -> true
    | `Response _ -> false

  let report_warning ~input st (T.Warning (env, fragment, w)) =
    let loc = T.fragment_loc env fragment in
    match w with
    | T.Shadowing (id, `Reserved ((`Solver _) as r), _) when typing_logic input ->
      error ~input ~loc st reserved (id, r)

    (* typer warnings that are actually errors given some languages spec *)
    | T.Shadowing (id, ((`Builtin `Term | `Not_found) as old), `Variable _)
    | T.Shadowing (id, ((`Constant _ | `Builtin _ | `Not_found) as old), `Constant _)
      when smtlib2_6_shadow_rules input ->
      error ~input ~loc st multiple_declarations (id, old)

    (* unused variables *)
    | T.Unused_type_variable (kind, v) ->
      if var_can_be_unused v then st
      else warn ~input ~loc st unused_type_variable (kind, v)
    | T.Unused_term_variable (kind, v) ->
      if var_can_be_unused v then st
      else warn ~input ~loc st unused_term_variable (kind, v)

    (* *)
    | T.Superfluous_destructor _ ->
      warn ~input ~loc st superfluous_destructor ()
    | T.Shadowing (id, old, _cur) ->
      warn ~input ~loc st shadowing (id, old)
    | T.Redundant_pattern pattern ->
      warn ~input ~loc st redundant_pattern pattern

    (* smtlib2 *)
    | Smtlib2_Core.Unknown_attribute id ->
      warn ~input ~loc st unknown_attribute id
    | Smtlib2_Arrays.Extension id ->
      warn ~input ~loc st array_extension id
    | Smtlib2_Ints.Restriction (config, msg)
    | Smtlib2_Reals.Restriction (config, msg)
    | Smtlib2_Reals_Ints.Restriction (config, msg) ->
      warn ~input ~loc st bad_arith_expr (config, msg)

    (* catch-all *)
    | _ ->
      warn ~input ~loc st unknown_warning
        (Obj.Extension_constructor.(name (of_val w)))

  (* Report type errors *)
  (* ************************************************************************ *)

  let report_error ~input st (T.Error (env, fragment, err)) =
    let loc = T.fragment_loc env fragment in
    match err with
    (* Datatype definition not well founded *)
    | T.Not_well_founded_datatypes _ ->
      error ~input ~loc st not_well_founded_datatype ()
    (* Generic error for when something was expected but not there *)
    | T.Expected (expect, got) ->
      error ~input ~loc st expect_error (expect, got)
    (* Arity errors *)
    | T.Bad_index_arity (s, expected, actual) ->
      error ~input ~loc st bad_index_arity (s, expected, actual)
    | T.Bad_ty_arity (c, actual) ->
      error ~input ~loc st bad_type_arity (c, actual)
    | T.Bad_op_arity (symbol, expected, actual) ->
      error ~input ~loc st bad_op_arity (symbol, expected, actual)
    | T.Bad_cstr_arity (c, expected, actual) ->
      error ~input ~loc st bad_cstr_arity (c, expected, actual)
    | T.Bad_term_arity (c, expected, actual) ->
      error ~input ~loc st bad_term_arity (c, expected, actual)
    | T.Bad_poly_arity (vars, args) ->
      error ~input ~loc st bad_poly_arity (vars, args)
    | T.Over_application over_args ->
      error ~input ~loc st over_application over_args
    (* Pattern matching errors *)
    | T.Partial_pattern_match missing ->
      error ~input ~loc st partial_pattern_match missing
    (* Record constuction errors *)
    | T.Repeated_record_field f ->
      error ~input ~loc st repeated_record_field f
    | T.Missing_record_field f ->
      error ~input ~loc st missing_record_field f
    | T.Mismatch_record_type (f, r) ->
      error ~input ~loc st mismatch_record_type (f, r)
    (* Application of a variable *)
    | T.Var_application v ->
      error ~input ~loc st var_application v
    | T.Ty_var_application v ->
      error ~input ~loc st ty_var_application v
    (* Wrong type *)
    | T.Type_mismatch (t, expected) ->
      error ~input ~loc st type_mismatch (t, expected)
    | T.Var_in_binding_pos_underspecified ->
      error ~input ~loc st var_in_binding_pos_underspecified ()
    | T.Unhandled_builtin b ->
      error ~input ~loc st unhandled_builtin b
    | T.Cannot_tag_tag ->
      error ~input ~loc st cannot_tag_tag ()
    | T.Cannot_tag_ttype ->
      error ~input ~loc st cannot_tag_ttype ()
    | T.Cannot_find (id, msg) ->
      error ~input ~loc st unbound_identifier (id, msg, true)
    | T.Forbidden_quantifier ->
      error ~input ~loc st forbidden_quant ()
    | T.Missing_destructor id ->
      error ~input ~loc st missing_destructor id
    | T.Type_def_rec def ->
      error ~input ~loc st type_def_rec def
    | T.Id_definition_conflict (id, binding) ->
      error ~input ~loc st id_definition_conflict (id, binding)
    | T.Higher_order_application ->
      error ~input ~loc st higher_order_app ()
    | T.Higher_order_type ->
      error ~input ~loc st higher_order_type ()
    | T.Higher_order_env_in_tff_typechecker ->
      error ~input ~loc st higher_order_env_in_tff_typer ()
    | T.Polymorphic_function_argument ->
      error ~input ~loc st poly_arg ()
    | T.Non_prenex_polymorphism ty ->
      error ~input ~loc st non_prenex_polymorphism ty
    | T.Inference_forbidden (_, w_src, inferred_ty) ->
      error ~input ~loc st inference_forbidden (env, w_src, inferred_ty)
    | T.Inference_conflict (_, w_src, inferred_ty, allowed_tys) ->
      error ~input ~loc st inference_conflict (env, w_src, inferred_ty, allowed_tys)
    | T.Inference_scope_escape (_, w_src, escaping_var, var_reason) ->
      error ~input ~loc st inference_scope_escape (env, w_src, escaping_var, var_reason)
    | T.Unbound_type_wildcards tys ->
      error ~input ~loc st unbound_type_wildcards (env, tys)
    | T.Incoherent_type_redefinition (id, cst, reason, n) ->
      error ~input ~loc st incoherent_type_redefinition (id, cst, reason, n)
    | T.Incoherent_term_redefinition (id, cst, reason, ty) ->
      error ~input ~loc st incoherent_term_redefinition (id, cst, reason, ty)
    | T.Inferred_builtin b ->
      error ~input ~loc st Report.Error.internal_error
        (Format.dprintf "Inferred_builtin %a" Dolmen.Std.Term.print_builtin b)
    | T.Forbidden_hook ->
      error ~input ~loc st Report.Error.internal_error
        (Format.dprintf "Forbidden Hook")
    | T.Unhandled_ast ->
      error ~input ~loc st unhandled_ast (env, fragment)
    (* Alt-Ergo Functional Array errors *)
    | Ae_Arrays.Bad_farray_arity ->
      error ~input ~loc st bad_farray_arity ()
    (* Alt-Ergo Bit-Vector errors *)
    | Ae_Bitv.Invalid_bin_char c ->
      error ~input ~loc st invalid_bin_bitvector_char c
    | Ae_Bitv.Expected_nat_lit t ->
      error ~input ~loc st bitvector_app_expected_nat_lit t
    (* Alt-Ergo Arithmetic errors *)
    | Ae_Arith.Expected_arith_type ty ->
      error ~input ~loc st expected_arith_type (ty, "")
    (* Tptp Arithmetic errors *)
    | Tptp_Arith.Expected_arith_type ty ->
      error ~input ~loc st expected_arith_type
        (ty, "Tptp arithmetic symbols are only polymorphic over the arithmetic \
              types $int, $rat and $real.")
    | Tptp_Arith.Cannot_apply_to ty ->
      error ~input ~loc st expected_specific_arith_type ty
    (* Smtlib Array errors *)
    | Smtlib2_Arrays.Forbidden msg ->
      error ~input ~loc st forbidden_array_sort msg
    (* Smtlib Arithmetic errors *)
    | Smtlib2_Ints.Forbidden (config, msg)
    | Smtlib2_Reals.Forbidden (config, msg)
    | Smtlib2_Reals_Ints.Forbidden (config, msg) ->
      error ~input ~loc st forbidden_arith_expr (config, msg)
    | Smtlib2_Reals_Ints.Expected_arith_type ty ->
      error ~input ~loc st expected_arith_type
        (ty, "The stmlib Reals_Ints theory requires an arithmetic type in order to \
              correctly desugar the expression.")
    (* Smtlib Bitvector errors *)
    | Smtlib2_Bitv.Non_positive_bitvector_size i ->
      error ~input ~loc st non_positive_bitv_size i
    | Smtlib2_Bitv.Invalid_extract (i, j, m) ->
      error ~input ~loc st invalid_smt2_bitv_extract (i, j, m)
    | Smtlib2_Bitv.Invalid_bin_char c
    | Smtlib2_Float.Invalid_bin_char c ->
      error ~input ~loc st invalid_bin_bitvector_char c
    | Smtlib2_Bitv.Invalid_hex_char c
    | Smtlib2_Float.Invalid_hex_char c ->
      error ~input ~loc st invalid_hex_bitvector_char c
    | Smtlib2_Bitv.Invalid_dec_char c
    | Smtlib2_Float.Invalid_dec_char c ->
      error ~input ~loc st invalid_dec_bitvector_char c
    (* Smtlib String errors *)
    | Smtlib2_String.Invalid_hexadecimal s ->
      error ~input ~loc st invalid_hex_string_char s
    | Smtlib2_String.Invalid_string_char c ->
      error ~input ~loc st invalid_string_char c
    | Smtlib2_String.Invalid_escape_sequence (s, i) ->
      error ~input ~loc st invalid_string_escape_sequence (s, i)
    (* Smtlib2 core errors *)
    | Smtlib2_Core.Incorrect_sexpression msg ->
      error ~input ~loc st incorrect_sexpression msg
    | Smtlib2_Core.Non_closed_named_term (ty_vars, t_vars) ->
      error ~input ~loc st non_closed_named_term (ty_vars, t_vars)
    (* Uncaught exception during type-checking *)
    | T.Uncaught_exn ((Alarm.Out_of_time |
                       Alarm.Out_of_space |
                       Pipeline.Sigint) as exn, bt) ->
      Printexc.raise_with_backtrace exn bt
    | T.Uncaught_exn (exn, bt) ->
      error ~input ~loc st Report.Error.uncaught_exn (exn, bt)
    (* Bad tptp kind *)
    | Bad_tptp_kind o ->
      error ~input ~loc st bad_tptp_kind o
    (* Missing smtlib logic *)
    | Missing_smtlib_logic ->
      error ~input ~loc st missing_smtlib_logic ()
    (* Illegal declarations *)
    | Illegal_decl ->
      error ~input ~loc st illegal_decl ()
    | Illegal_def ->
      error ~input ~loc st illegal_def ()
    (* Push/Pop errors *)
    | Invalid_push_n ->
      error ~input ~loc st invalid_push ()
    | Invalid_pop_n ->
      error ~input ~loc st invalid_pop ()
    | Pop_with_empty_stack ->
      error ~input ~loc st empty_pop ()
    (* Catch-all *)
    | _ ->
      error ~input ~loc st unknown_error
        (Obj.Extension_constructor.(name (of_val err)))

  (* Some misc functions *)
  (* ************************************************************************ *)

  let pop_inferred_model_constants st =
    let key = Smtlib2_Core.inferred_model_constants in
    let state = (State.get ty_state st).typer in
    let res =
      match T.get_global_custom_state state key with
      | None -> []
      | Some l -> l
    in
    let () = T.set_global_custom_state state key [] in
    res


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
          Smtlib2_Arrays.parse ~config:l.features.arrays v :: acc
        | `Ints ->
          Smtlib2_Ints.parse ~config:l.features.arithmetic v :: acc
        | `Reals ->
          Smtlib2_Reals.parse ~config:l.features.arithmetic v :: acc
        | `Reals_Ints ->
          Smtlib2_Reals_Ints.parse ~config:l.features.arithmetic v :: acc
      ) [] l.Dolmen_type.Logic.Smtlib2.theories

  let typing_env ?(attrs=[]) ~loc warnings (st : State.t) (input : input) =
    let file = file_loc_of_input input in

    (* Match the language to determine bultins and other options *)
    let lang =
      match lang_of_input input with
      | #lang as lang -> lang
      | `Missing ->
        let st =
          error st ~input ~loc:{ file; loc; } Report.Error.internal_error
            (Format.dprintf "Missing input language \
                             (should have been set during parsing)")
        in
        raise (State.Error st)
    in
    let user_builtins =
      let additional_builtins = State.get additional_builtins st st lang in
      let extension_builtins =
        List.map (fun ext -> Ext.builtins ext lang) (State.get extension_builtins st)
      in
      additional_builtins :: extension_builtins
    in
    match lang with
    (* Dimacs & iCNF
       - these infer the declarations of their constants
         (we could declare them when the number of clauses and variables
         is declared in the file, but it's easier this way).
       - there are no explicit declaration or definitions, hence no builtins *)
    | `Logic Dimacs | `Logic ICNF ->
      let poly = T.Flexible in
      let var_infer = T.{
          var_hook = ignore;
          infer_unbound_vars = No_inference;
          infer_type_vars_in_binding_pos = false;
          infer_term_vars_in_binding_pos = No_inference;
        } in
      let sym_infer = T.{
          sym_hook = ignore;
          infer_type_csts = false;
          infer_term_csts = Wildcard (Any_base {
              allowed = [Dolmen.Std.Expr.Ty.prop];
              preferred = Dolmen.Std.Expr.Ty.prop;
            });
        } in
      let builtins = Dolmen_type.Base.merge (
          user_builtins @ [
          Dimacs.parse
        ]) in
      T.empty_env ~order:First_order
        ~st:(State.get ty_state st).typer
        ~var_infer ~sym_infer ~poly
        ~warnings ~file builtins

    (* Alt-Ergo format *)
    | `Logic Alt_ergo ->
      let poly = T.Flexible in
      let free_wildcards = T.Implicitly_universally_quantified in
      let var_infer = T.{
          var_hook = ignore;
          infer_unbound_vars = Unification_type_variable;
          infer_type_vars_in_binding_pos = true;
          infer_term_vars_in_binding_pos = No_inference;
        } in
      let sym_infer = T.{
          sym_hook = ignore;
          infer_type_csts = false;
          infer_term_csts = No_inference;
        } in
      let builtins = Dolmen_type.Base.merge (
          user_builtins @ [
          Ae_Core.parse;
          Ae_Arith.parse;
          Ae_Arrays.parse;
          Ae_Bitv.parse;
          Ae_Float.parse;
        ]) in
      T.empty_env ~order:First_order
        ~st:(State.get ty_state st).typer
        ~var_infer ~sym_infer ~poly
        ~free_wildcards ~warnings ~file
        builtins

    (* Zipperposition Format
       - no inference of constants
       - base + arith builtins
    *)
    | `Logic Zf ->
      let poly = T.Flexible in
      let var_infer = T.{
          var_hook = ignore;
          infer_unbound_vars = No_inference;
          infer_type_vars_in_binding_pos = true;
          infer_term_vars_in_binding_pos = Wildcard Any_in_scope;
        } in
      let sym_infer = T.{
          sym_hook = ignore;
          infer_type_csts = false;
          infer_term_csts = No_inference;
        } in
      let builtins = Dolmen_type.Base.merge (
          user_builtins @ [
          Zf_Core.parse;
          Zf_arith.parse
        ]) in
      T.empty_env ~order:Higher_order
        ~st:(State.get ty_state st).typer
        ~var_infer ~sym_infer ~poly
        ~warnings ~file builtins

    (* TPTP
       - tptp has inference of constants
       - 2 base theories (Core and Arith)
    *)
    | `Logic Tptp v ->
      let poly = T.Explicit in
      begin match tptp_kind_of_attrs attrs with
        | Some "thf" ->
          let var_infer = T.{
              var_hook = ignore;
              infer_unbound_vars = No_inference;
              infer_type_vars_in_binding_pos = true;
              infer_term_vars_in_binding_pos = No_inference;
            } in
          let sym_infer = T.{
              sym_hook = ignore;
              infer_type_csts = false;
              infer_term_csts = No_inference;
            } in
          let builtins = Dolmen_type.Base.merge (
              user_builtins @ [
              Tptp_Core_Ho.parse v;
              Tptp_Arith.parse v;
            ]) in
          T.empty_env ~order:Higher_order
            ~st:(State.get ty_state st).typer
            ~var_infer ~sym_infer ~poly
            ~warnings ~file:file builtins
        | Some ("tff" | "tpi" | "fof" | "cnf") ->
          let var_infer = T.{
              var_hook = ignore;
              infer_unbound_vars = No_inference;
              infer_type_vars_in_binding_pos = true;
              infer_term_vars_in_binding_pos =
                Wildcard (Any_base {
                    allowed = [Dolmen.Std.Expr.Ty.base];
                    preferred = Dolmen.Std.Expr.Ty.base;
                  });
            } in
          let sym_infer = T.{
              sym_hook = ignore;
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
          let builtins = Dolmen_type.Base.merge (
              user_builtins @ [
              Tptp_Core.parse v;
              Tptp_Arith.parse v;
            ]) in
          T.empty_env ~order:First_order
            ~st:(State.get ty_state st).typer
            ~var_infer ~sym_infer ~poly
            ~warnings ~file builtins
        | bad_kind ->
          let builtins = Dolmen_type.Base.noop in
          let env =
            T.empty_env
              ~st:(State.get ty_state st).typer
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
    | `Logic Smtlib2 v ->
      let poly = T.Implicit in
      let var_infer = T.{
          var_hook = ignore;
          infer_unbound_vars = No_inference;
          infer_type_vars_in_binding_pos = true;
          infer_term_vars_in_binding_pos = No_inference;
        } in
      let sym_infer = T.{
          sym_hook = ignore;
          infer_type_csts = false;
          infer_term_csts = No_inference;
        } in
      begin match (State.get ty_state st).logic with
        | Auto ->
          let builtins = Dolmen_type.Base.noop in
          let env =
            T.empty_env ~order:First_order
              ~st:(State.get ty_state st).typer
              ~var_infer ~sym_infer ~poly
              ~warnings ~file builtins
          in
          T._error env (Located loc) Missing_smtlib_logic
        | Smtlib2 logic ->
          let builtins = Dolmen_type.Base.merge (
              user_builtins @
              builtins_of_smtlib2_logic (`Script v) logic
            ) in
          let quants = logic.features.quantifiers in
          T.empty_env ~order:First_order
            ~st:(State.get ty_state st).typer
            ~var_infer ~sym_infer ~poly ~quants
            ~warnings ~file builtins
      end
    | `Response Smtlib2 v ->
      let poly = T.Implicit in
      let var_infer = T.{
          var_hook = ignore;
          infer_unbound_vars = No_inference;
          infer_type_vars_in_binding_pos = true;
          infer_term_vars_in_binding_pos = No_inference;
        } in
      let sym_infer = T.{
          sym_hook = ignore;
          infer_type_csts = false;
          infer_term_csts = No_inference;
        } in
      begin match (State.get ty_state st).logic with
        | Auto ->
          let builtins = Dolmen_type.Base.noop in
          let env =
            T.empty_env ~order:First_order
              ~st:(State.get ty_state st).typer
              ~var_infer ~sym_infer ~poly
              ~warnings ~file builtins
          in
          T._error env (Located loc) Missing_smtlib_logic
        | Smtlib2 logic ->
          let builtins = Dolmen_type.Base.merge (
              user_builtins @
              builtins_of_smtlib2_logic (`Response v) logic
            ) in
          let quants = logic.features.quantifiers in
          T.empty_env ~order:First_order
            ~st:(State.get ty_state st).typer
            ~var_infer ~sym_infer ~poly ~quants
            ~warnings ~file builtins
      end

  let typing_wrap ?attrs ?(loc=Dolmen.Std.Loc.no_loc) ~input st ~f =
    let st = ref st in
    let report w = st := report_warning ~input !st w in
    match f (typing_env ?attrs ~loc report !st input) with
    | res -> !st, res
    | exception T.Typing_error err ->
      let st = report_error ~input !st err in
      raise (State.Error st)


  (* Push&Pop *)
  (* ************************************************************************ *)

  let reset st ?loc:_ () =
    State.set ty_state (new_state ()) st

  let reset_assertions st ?loc:_ () =
    let state = State.get ty_state st in
    State.set ty_state {
      logic = state.logic;
      logic_loc = state.logic_loc;
      typer = T.new_state ();
      stack = [];
    } st

  let rec push st ~input ?(loc=Dolmen.Std.Loc.no_loc) = function
    | 0 -> st
    | i ->
      if i < 0 then
        fst @@ typing_wrap ~input ~loc st ~f:(fun env ->
            T._error env (Located loc) Invalid_push_n
          )
      else begin
        let t = State.get ty_state st in
        let st' = T.copy_state t.typer in
        let t' = { t with stack = st' :: t.stack; } in
        let st' = State.set ty_state t' st in
        push st' ~input ~loc (i - 1)
      end

  let rec pop st ~input ?(loc=Dolmen.Std.Loc.no_loc) = function
    | 0 -> st
    | i ->
      if i < 0 then
        fst @@ typing_wrap ~loc st ~input ~f:(fun env ->
            T._error env (Located loc) Invalid_pop_n
          )
      else begin
        let t = State.get ty_state st in
        match t.stack with
        | [] ->
          fst @@ typing_wrap ~input ~loc st ~f:(fun env ->
              T._error env (Located loc) Pop_with_empty_stack
            )
        | ty :: r ->
          let t' = { t with typer = ty; stack = r; } in
          let st' = State.set ty_state t' st in
          pop st' ~input ~loc (i - 1)
      end


  (* Setting the logic *)
  (* ************************************************************************ *)

  let set_logic_aux ~input ~loc st new_logic =
    let ty_st = State.get ty_state st in
    let st =
      match ty_st.logic with
      | Auto -> st
      | Smtlib2 _ -> warn ~input ~loc st logic_reset ty_st.logic_loc
    in
    State.set ty_state {
      ty_st with
      logic = new_logic;
      logic_loc = Dolmen.Std.Loc.full_loc loc;
    } st

  let set_logic (st : State.t) ~input ?(loc=Dolmen.Std.Loc.no_loc) s =
    let file_loc = file_loc_of_input input in
    let loc : Dolmen.Std.Loc.full = { file = file_loc; loc; } in
    match lang_of_input input with
    | `Logic ICNF -> st, Dolmen_type.Logic.Auto
    | `Logic Dimacs -> st, Dolmen_type.Logic.Auto
    | `Logic Smtlib2 _ ->
      let logic =
        match State.get smtlib2_forced_logic st with
        | None -> s
        | Some forced_logic -> forced_logic
      in
      let st, l =
        match Dolmen_type.Logic.Smtlib2.parse logic with
        | Some l -> st, l
        | None ->
          let st = warn ~input ~loc st unknown_logic s in
          st, Dolmen_type.Logic.Smtlib2.all
      in
      let new_logic = Dolmen_type.Logic.Smtlib2 l in
      let st = set_logic_aux ~input ~loc st new_logic in
      st, new_logic
    | _ ->
      let st = warn ~input ~loc st set_logic_not_supported () in
      st, Dolmen_type.Logic.Auto

  (* Some types *)
  (* ************************************************************************ *)

  type decl = [
    | `Term_decl of Dolmen_std.Expr.ty Dolmen_std.Expr.id
    | `Type_decl of
        Dolmen_std.Expr.type_fun Dolmen_std.Expr.id *
        Dolmen_std.Expr.ty_def option
  ]

  type def = [
    | `Instanceof of
        Dolmen_std.Id.t * Dolmen_std.Expr.ty Dolmen_std.Expr.id *
        Dolmen_std.Expr.ty list *
        Dolmen_std.Expr.type_ Dolmen_std.Expr.id list *
        Dolmen_std.Expr.ty Dolmen_std.Expr.id list * Dolmen_std.Expr.term
    | `Term_def of
        Dolmen_std.Id.t * Dolmen_std.Expr.ty Dolmen_std.Expr.id *
        Dolmen_std.Expr.type_ Dolmen_std.Expr.id list *
        Dolmen_std.Expr.ty Dolmen_std.Expr.id list * Dolmen_std.Expr.term
    | `Type_alias of
        Dolmen_std.Id.t * Dolmen_std.Expr.type_fun Dolmen_std.Expr.id *
        Dolmen_std.Expr.type_ Dolmen_std.Expr.id list * Dolmen_std.Expr.ty
  ]

  type 'a ret = {
    implicit_decls : decl list;
    implicit_defs : def list;
    ret : 'a;
  }

  (* Declarations helpers *)
  let allow_function_decl (st : State.t) =
    match (State.get ty_state st).logic with
    | Smtlib2 logic -> logic.features.free_functions
    | Auto -> true

  let allow_data_type_decl (st : State.t) =
    match (State.get ty_state st).logic with
    | Smtlib2 logic -> logic.features.datatypes
    | Auto -> true

  let allow_abstract_type_decl (st : State.t) =
    match (State.get ty_state st).logic with
    | Smtlib2 logic -> logic.features.free_sorts
    | Auto -> true

  let check_decl st env d = function
    | `Type_decl (_, ty_def) ->
      begin match (ty_def : Dolmen.Std.Expr.Ty.def option) with
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

  let tr_decl st env parsed_decl typed_decl =
    let () = check_decl st env parsed_decl typed_decl in
    typed_decl

  (* Definitions helpers, this function should only be called in the context
     of a typing_wrap function. *)
  let tr_def env ~implicit ~recursive typed_def =
    match typed_def with
    | `Type_alias (loc, id, c, vars, body) ->
      if not recursive && not implicit then begin
        Dolmen.Std.Expr.Ty.alias_to c vars body;
        `Type_alias (id, c, vars, body)
      end else
        T._error env (Located loc) Illegal_def
    | `Term_def (_loc, id, f, vars, params, body) ->
      `Term_def (id, f, vars, params, body)
    | `Instanceof (_loc, id, f, ty_args, vars, params, body) ->
      `Instanceof (id, f, ty_args, vars, params, body)

  let empty_ret ret =
    { implicit_decls = [];
      implicit_defs = [];
      ret; }

  let mk_ret env ~f (ret : _ T.ret) =
    let implicit_decls = ret.implicit_decls in
    let implicit_defs =
      List.map (tr_def env ~implicit:true ~recursive:false) ret.implicit_defs
    in
    let ret = f ret.result in
    { implicit_decls; implicit_defs; ret; }

  let merge_rets l =
    let implicit_decls =
      Dolmen_std.Misc.list_concat_map (fun r -> r.implicit_decls) l
    in
    let implicit_defs =
      Dolmen_std.Misc.list_concat_map (fun r -> r.implicit_defs) l
    in
    let ret = List.map (fun r -> r.ret) l in
    { implicit_decls; implicit_defs; ret; }

  (* Declarations *)
  (* ************************************************************************ *)

  let decls (st : State.t) ~input ?loc ?attrs d : state * decl list ret =
    typing_wrap ?attrs ?loc ~input st ~f:(fun env ->
        let ret_decls = T.decls env ?attrs d in
        mk_ret env ret_decls ~f:(List.map2 (tr_decl st env) d.contents)
      )

  (* Definitions *)
  (* ************************************************************************ *)

  let defs ~mode st ~input ?loc ?attrs d : state * def list ret =
    typing_wrap ?attrs ?loc ~input st ~f:(fun env ->
        let ret_defs = T.defs ~mode env ?attrs d in
        mk_ret env ret_defs ~f:(List.map (tr_def env ~implicit:false ~recursive:d.recursive))
      )

  (* Wrappers around the Type-checking module *)
  (* ************************************************************************ *)

  let terms st ~input ?loc ?attrs l : state * _ ret =
    match l with
    | [] -> st, empty_ret []
    | _ ->
      typing_wrap ?attrs ?loc ~input st
        ~f:(fun env ->
            let ret_l = List.map (T.term env) l in
            let rets = List.map (mk_ret env ~f:(fun t -> t)) ret_l in
            merge_rets rets
          )

  let formula st ~input ?loc ?attrs ~goal:_ t : state * _ ret =
    typing_wrap ?attrs ?loc ~input st
      ~f:(fun env ->
          let ret_f = T.formula env t in
          mk_ret env ret_f ~f:(fun f -> f)
        )

  let formulas st ~input ?loc ?attrs l : state * _ ret =
    match l with
    | [] -> st, empty_ret []
    | _ ->
      typing_wrap ?attrs ?loc ~input st
        ~f:(fun env ->
            let ret_l = List.map (T.formula env) l in
            let rets = List.map (mk_ret env ~f:(fun t -> t)) ret_l in
            merge_rets rets
          )

end


(* Pipes functor *)
(* ************************************************************************ *)

module type Typer = Typer_intf.Typer

module type S = Typer_intf.S

module Make
    (Expr : Expr_intf.S)
    (Print : Expr_intf.Print
     with type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type ty_def := Expr.ty_def
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
    (State : State.S)
    (Typer : Typer
     with type state := State.t
      and type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type ty_def := Expr.ty_def
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
= struct

  module S = Dolmen.Std.Statement

  let pipe = "Typer_pipe"
  let type_check : bool State.key = State.create_key ~pipe "type_check"

  let init
      ~type_check:type_check_value
      st =
    st
    |> State.set type_check type_check_value

  (* Types used in Pipes *)
  (* ************************************************************************ *)

  type env = Typer.env

  (* Used for representing typed statements *)
  type +'a stmt = {
    id        : Dolmen.Std.Id.t;
    loc       : Dolmen.Std.Loc.t;
    contents  : 'a;
    attrs     : Dolmen.Std.Term.t list;
    implicit  : bool;
  }

  type def = [
    | `Type_alias of Dolmen.Std.Id.t * Expr.ty_cst * Expr.ty_var list * Expr.ty
    | `Term_def of Dolmen.Std.Id.t * Expr.term_cst * Expr.ty_var list * Expr.term_var list * Expr.term
    | `Instanceof of Dolmen.Std.Id.t * Expr.term_cst * Expr.ty list * Expr.ty_var list * Expr.term_var list * Expr.term
  ]

  type defs = [
    | `Defs of def list
  ]

  type decl = [
    | `Type_decl of Expr.ty_cst * Expr.ty_def option
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
    | `Solve of Expr.formula list * Expr.formula list
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
    | `Other of Dolmen.Std.Id.t * Dolmen.Std.Statement.term list
  ]

  type set_info = [
    | `Set_logic of string * Dolmen_type.Logic.t
    | `Set_info of Dolmen.Std.Statement.term
    | `Set_option of Dolmen.Std.Statement.term
  ]

  type stack_control = [
    | `Pop of int
    | `Push of int
    | `Reset_assertions
    | `Reset
  ]

  type exit = [
    | `Exit
  ]

  (* Agregate types *)
  type typechecked = [ defs | decls | assume | solve | get_info | set_info | stack_control | exit ]

  (* Simple constructor *)
  let mk_stmt ?(implicit=false) id loc attrs (contents: typechecked) =
    { id; loc; attrs; contents; implicit; }

  let print_def fmt = function
    | `Type_alias (id, c, vars, body) ->
      Format.fprintf fmt "@[<hov 2>type-alias:@ %a: %a(%a) ->@ %a@]"
        Dolmen.Std.Id.print id Print.ty_cst c
        (Format.pp_print_list Print.ty_var) vars Print.ty body
    | `Term_def (id, c, vars, args, body) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt
        "@[<hv 2>term-def{%a}:@ @[<hv>%a@] =@ @[<hov 2>fun (%a;@ %a) ->@ %a@]@]"
        Dolmen.Std.Id.print id Print.term_cst c
        (Format.pp_print_list ~pp_sep Print.ty_var) vars
        (Format.pp_print_list ~pp_sep Print.term_var) args
        Print.term body
    | `Instanceof (id, c, ty_args, vars, args, body) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt
        "@[<hv 2>instanceof{%a}:@ @[<hv>%a(%a)@] =@ @[<hov 2>fun (%a;@ %a) ->@ %a@]@]"
        Dolmen.Std.Id.print id Print.term_cst c
        (Format.pp_print_list ~pp_sep Print.ty) ty_args
        (Format.pp_print_list ~pp_sep Print.ty_var) vars
        (Format.pp_print_list ~pp_sep Print.term_var) args
        Print.term body

  let print_ty_def fmt = function
    | None -> ()
    | Some ty_def ->
      Format.fprintf fmt " =@ %a" Print.ty_def ty_def

  let print_decl fmt = function
    | `Type_decl (c, ty_def) ->
      Format.fprintf fmt "@[<hov 2>type-def:@ %a%a@]"
        Print.ty_cst c print_ty_def ty_def
    | `Term_decl c ->
      Format.fprintf fmt "@[<hov 2>term-decl:@ %a@]" Print.term_cst c

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
    | `Solve (hyps, goals) ->
      Format.fprintf fmt "@[<hov 2>solve: %a@ assuming: %a@]"
        (Format.pp_print_list Print.formula) goals
        (Format.pp_print_list Print.formula) hyps
    | `Get_info s ->
      Format.fprintf fmt "@[<hov 2>get-info: %s@]" s
    | `Get_option s ->
      Format.fprintf fmt "@[<hov 2>get-option: %s@]" s
    | `Get_proof ->
      Format.fprintf fmt "@[<hov 2>get-proof@]"
    | `Get_unsat_core ->
      Format.fprintf fmt "@[<hov 2>get-unsat-core@]"
    | `Get_unsat_assumptions ->
      Format.fprintf fmt "@[<hov 2>get-unsat-assumptions@]"
    | `Get_model ->
      Format.fprintf fmt "@[<hov 2>get-model@]"
    | `Get_value l ->
      Format.fprintf fmt "@[<v 2>get-value:@ %a@]"
        (Format.pp_print_list Print.term) l
    | `Get_assignment ->
      Format.fprintf fmt "@[<hov 2>get-assignment@]"
    | `Get_assertions ->
      Format.fprintf fmt "@[<hov 2>get-assertions@]"
    | `Echo s ->
      Format.fprintf fmt "@[<hov 2>echo: %s@]" s
    | `Other (name, args) ->
      Format.fprintf fmt "@[<hov 2>other/%a: %a@]"
        Dolmen.Std.Id.print name
        (Format.pp_print_list Dolmen.Std.Term.print) args
    | `Set_logic (s, logic) ->
      Format.fprintf fmt
        "@[<hov 2>set-logic: %s =@ %a@]"
        s Dolmen_type.Logic.print logic
    | `Set_info t ->
      Format.fprintf fmt "@[<hov 2>set-info: %a@]"
        Dolmen.Std.Term.print t
    | `Set_option t ->
      Format.fprintf fmt "@[<hov 2>set-option: %a@]"
        Dolmen.Std.Term.print t
    | `Pop n ->
      Format.fprintf fmt "@[<hov 2>pop: %d@]" n
    | `Push n ->
      Format.fprintf fmt "@[<hov 2>push: %d@]" n
    | `Reset_assertions ->
      Format.fprintf fmt "@[<hov 2>reset-assertions@]"
    | `Reset ->
      Format.fprintf fmt "@[<hov 2>reset@]"
    | `Exit ->
      Format.fprintf fmt "@[<hov 2>exit@]"

  let print_attr fmt t =
    Format.fprintf fmt "{%a}@," Dolmen.Std.Term.print t

  let print_attrs fmt attrs =
    List.iter (print_attr fmt) attrs

  let print fmt ({ id; loc; attrs; contents; implicit = _; } : typechecked stmt) =
    Format.fprintf fmt "@[<v 2>%a[%a]:@,%a%a@]"
      Dolmen.Std.Id.print id
      Dolmen.Std.Loc.print_compact loc
      print_attrs attrs
      print_typechecked contents

  (* Typechecking *)
  (* ************************************************************************ *)

  let new_stmt_id ref_name =
    let counter = ref 0 in
    (fun () ->
       let () = incr counter in
       let name = Format.sprintf "%s_%d" ref_name !counter in
       Dolmen.Std.Id.mk Dolmen.Std.Id.decl name
    )

  let stmt_id ref_name =
    let new_id = new_stmt_id ref_name in
    (fun c ->
       match c.Dolmen.Std.Statement.id with
       | None -> new_id ()
       | Some id -> id)

  let def_id   = stmt_id "def"
  let decl_id  = stmt_id "decl"
  let hyp_id   = stmt_id "hyp"
  let goal_id  = stmt_id "goal"
  let prove_id = stmt_id "prove"
  let other_id = stmt_id "other"

  let implicit_decl_name = new_stmt_id "implicit_decl"
  let implicit_def_name = new_stmt_id "implicit_def"

  let implicits loc attrs ({ implicit_decls; implicit_defs; ret = _; } : _ Typer.ret) =
    let decls =
      List.map (fun d ->
          mk_stmt ~implicit:true (implicit_decl_name ()) loc attrs (`Decls [d])
        ) implicit_decls
    in
    let defs =
      List.map (fun d ->
          mk_stmt ~implicit:true (implicit_def_name ()) loc attrs (`Defs [d])
        ) implicit_defs
    in
    decls @ defs

  let fv_list l =
    let l' = List.map Dolmen.Std.Term.fv l in
    List.sort_uniq Dolmen.Std.Id.compare (List.flatten l')

  let quantify ~loc var_ty vars f =
    let vars = List.map (fun v ->
        let c = Dolmen.Std.Term.const ~loc v in
        match var_ty v with
        | None -> c
        | Some ty -> Dolmen.Std.Term.colon c ty
      ) vars in
    Dolmen.Std.Term.forall ~loc vars f

  let normalize _st c =
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
            | [] -> Dolmen.Std.Term.false_ ~loc ()
            | [p] -> p
            | _ -> Dolmen.Std.Term.apply ~loc (Dolmen.Std.Term.or_t ~loc ()) l
          in
          let f = quantify ~loc (fun _ -> None) free_vars f in
          { c with descr = S.Antecedent f; }
      end

    (* catch all *)
    | _ -> c

  let check st c =
    let input = `Logic (State.get State.logic_file st) in
    match normalize st c with
    (* State&Assertion stack management *)
    | { S.descr = S.Reset; loc; attrs; _ } ->
      let st = Typer.reset st ~loc () in
      st, [mk_stmt (other_id c) loc attrs `Reset]
    | { S.descr = S.Pop i; loc; attrs; _ } ->
      let st = Typer.pop st ~input ~loc i in
      st, [mk_stmt (other_id c) loc attrs (`Pop i)]
    | { S.descr = S.Push i; loc; attrs; _ } ->
      let st = Typer.push st ~input ~loc i in
      st, [mk_stmt (other_id c) loc attrs (`Push i)]
    | { S.descr = S.Reset_assertions; loc; attrs; _ } ->
      let st = Typer.reset_assertions st ~loc () in
      st, [mk_stmt (other_id c) loc attrs `Reset_assertions]

    (* Plain statements
       TODO: allow the `plain` function to return a meaningful value *)
    | { S.descr = S.Other { name; args; }; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs (`Other (name, args))]

    (* Prove statements (with local hyps/goals) *)
    | { S.descr = S.Prove { hyps; goals }; loc; attrs; _ } ->
      let st, h_ret = Typer.formulas st ~input ~loc ~attrs hyps in
      let st, g_ret = Typer.formulas st ~input ~loc ~attrs goals in
      let prove = mk_stmt (prove_id c) loc attrs (`Solve (h_ret.ret, g_ret.ret)) in
      st, (implicits loc attrs h_ret @
           implicits loc attrs g_ret @
           [prove])

    (* Hypotheses & Goals *)
    | { S.descr = S.Clause l; loc; attrs; _ } ->
      let st, res = Typer.formulas st ~input ~loc ~attrs l in
      let clause : typechecked stmt = mk_stmt (hyp_id c) loc attrs (`Clause res.ret) in
      st, (implicits loc attrs res @ [clause])
    | { S.descr = S.Antecedent t; loc; attrs; _ } ->
      let st, f = Typer.formula st ~input ~loc ~attrs ~goal:false t in
      let hyp : typechecked stmt = mk_stmt (hyp_id c) loc attrs (`Hyp f.ret) in
      st, (implicits loc attrs f @ [hyp])
    | { S.descr = S.Consequent t; loc; attrs; _ } ->
      let st, f = Typer.formula st ~input ~loc ~attrs ~goal:true t in
      let goal : typechecked stmt = mk_stmt (goal_id c) loc attrs (`Goal f.ret) in
      st, (implicits loc attrs f @ [goal])

    (* Other set_logics should check whether corresponding plugins are activated ? *)
    | { S.descr = S.Set_logic s; loc; attrs; _ } ->
      let st, new_logic = Typer.set_logic st ~input ~loc s in
      st, [mk_stmt (other_id c) loc attrs (`Set_logic (s, new_logic))]

    (* Set/Get info *)
    | { S.descr = S.Get_info s; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs (`Get_info s)]
    | { S.descr = S.Set_info t; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs (`Set_info t)]

    (* Set/Get options *)
    | { S.descr = S.Get_option s; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs (`Get_option s)]
    | { S.descr = S.Set_option t; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs (`Set_option t)]

    (* Declarations and definitions *)
    | { S.descr = S.Decls l; loc; attrs; _ } ->
      let st, res = Typer.decls st ~input ~loc ~attrs l in
      let decls : typechecked stmt = mk_stmt (decl_id c) loc attrs (`Decls res.ret) in
      st, (implicits loc attrs res @ [decls])
    | { S.descr = S.Defs d; loc; attrs; _ } ->
      let st, res = Typer.defs ~mode:`Create_id st ~input ~loc ~attrs d in
      let defs : typechecked stmt = mk_stmt (def_id c) loc attrs (`Defs res.ret) in
      st, (implicits loc attrs res @ [defs])

    (* Smtlib's proof/model instructions *)
    | { S.descr = S.Get_proof; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs `Get_proof]
    | { S.descr = S.Get_unsat_core; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs `Get_unsat_core]
    | { S.descr = S.Get_unsat_assumptions; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs `Get_unsat_assumptions]
    | { S.descr = S.Get_model; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs `Get_model]
    | { S.descr = S.Get_value l; loc; attrs; _ } ->
      let st, res = Typer.terms st ~input ~loc ~attrs l in
      st, (implicits loc attrs res @ [mk_stmt (other_id c) loc attrs (`Get_value res.ret)])
    | { S.descr = S.Get_assignment; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs `Get_assignment]
    (* Assertions *)
    | { S.descr = S.Get_assertions; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs `Get_assertions]
    (* Misc *)
    | { S.descr = S.Echo s; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs (`Echo s)]
    | { S.descr = S.Exit; loc; attrs; _ } ->
      st, [mk_stmt (other_id c) loc attrs `Exit]

    (* packs and includes *)
    | { S.descr = S.Include _; _ } -> assert false
    | { S.descr = S.Pack _; _ } -> assert false

  let typecheck st c =
    let res =
      if not (State.get type_check st) then
        st, `Done ()
      else
        match c with
        (* Pack and includes.
           These should have been filtered out before this point.
           TODO: emit some kind of warning ? *)
        | { S.descr = S.Pack _; _ } -> st, `Done ()
        | { S.descr = S.Include _; _ } -> st, `Done ()

        (* all other statements *)
        | _ ->
          let st, res = check st c in
          st, `Continue res
    in
    res

end
