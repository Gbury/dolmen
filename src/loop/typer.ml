
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Dolmen_type functors instantiation *)
(* ************************************************************************ *)

module T = Dolmen_type.Tff.Make
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

(* Tptp builtins *)
module Tptp_Core =
  Dolmen_type.Core.Tptp.Tff(T)
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

(* Typing state *)
(* ************************************************************************ *)

(* This is here to define the typing state (not to confuse with the state
   passed in the pipes, which will contain the typing state. *)

type ty_state = {
  (* logic used *)
  logic : Dolmen_type.Logic.t;
  (* current typechecker global state *)
  typer : T.state;
  (* typechecker state stack *)
  stack : T.state list;
}

let new_state () = {
  logic = Auto;
  typer = T.new_state ();
  stack = [];
}


(* Make functor *)
(* ************************************************************************ *)

module type S = Typer_intf.S

module Make(S : State_intf.Typer with type ty_state := ty_state) = struct

  (* New warnings & errors *)
  (* ************************************************************************ *)

  let no_loc = Dolmen.Std.ParseLocation.mk "" 0 0 0 0

  type _ T.err +=
    | Warning_as_error : T.warning -> _ T.err
    | Missing_logic : Dolmen.Std.ParseLocation.t T.err
    | Illegal_decl : Dolmen.Std.Statement.decl T.err
    | Invalid_push_n : Dolmen.Std.ParseLocation.t T.err
    | Invalid_pop_n : Dolmen.Std.ParseLocation.t T.err
    | Pop_with_empty_stack : Dolmen.Std.ParseLocation.t T.err

  (* Hints for type errors *)
  (* ************************************************************************ *)

  let poly_hint fmt (c, expected, actual) =
    let n_ty, n_t = Dolmen.Std.Expr.Term.Const.arity c in
    let total_arity = n_ty + n_t in
    match expected with
    | [x] when x = total_arity && actual = n_t ->
      Format.fprintf fmt
        "@ @[<hov>Hint: %a@]" Format.pp_print_text
        "this is a polymorphic function, you probably forgot \
         the type arguments@]"
    | [x] when x = n_t && n_ty <> 0 ->
      Format.fprintf fmt "@ @[<hov>Hint: %a@]" Format.pp_print_text
        "it looks like the language enforces implicit polymorphism, \
         i.e. no type arguments are to be provided to applications \
         (and instead type annotation/coercions should be used)."
    | _ :: _ ->
      Format.fprintf fmt "@ @[<hov>Hint: %a@]" Format.pp_print_text
        "this is a polymorphic function, and multiple accepted arities \
         are possible because the language supports inference of all type \
         arguments when none are given in an application."
    | _ -> ()

  let pp_hint fmt = function
    | "" -> ()
    | msg ->
      Format.fprintf fmt "@ @[<hov 2>Hint: %a@]"
        Format.pp_print_text msg

  (* Report type warnings *)
  (* ************************************************************************ *)

  let pp_opt_loc fmt = function
    | None -> Format.fprintf fmt "<location missing>"
    | Some loc -> Dolmen.Std.ParseLocation.fmt_pos fmt loc

  let decl_loc d =
    match (d : Dolmen.Std.Statement.decl) with
    | Record { loc; _ }
    | Abstract { loc; _ }
    | Inductive { loc; _ } -> loc

  let print_reason fmt r =
    match (r : T.reason) with
    | Builtin ->
      Format.fprintf fmt "defined by a builtin theory"
    | Bound ast ->
      Format.fprintf fmt "bound at %a" pp_opt_loc ast.loc
    | Inferred ast ->
      Format.fprintf fmt "inferred at %a" pp_opt_loc ast.loc
    | Defined d ->
      Format.fprintf fmt "defined at %a" pp_opt_loc d.loc
    | Declared d ->
      Format.fprintf fmt "declared at %a" pp_opt_loc (decl_loc d)

  let print_reason_opt fmt = function
    | Some r -> print_reason fmt r
    | None -> Format.fprintf fmt "<location missing>"

  let report_warning (T.Warning (_env, _fragment, warn)) =
    match warn with
    | T.Unused_type_variable v -> Some (fun fmt () ->
        Format.fprintf fmt
          "Quantified type variable `%a` is unused"
          Dolmen.Std.Expr.Print.ty_var v
      )
    | T.Unused_term_variable v -> Some (fun fmt () ->
        Format.fprintf fmt
          "Quantified term variable `%a` is unused"
          Dolmen.Std.Expr.Print.term_var v
      )
    | T.Error_in_attribute exn -> Some (fun fmt () ->
        Format.fprintf fmt
          "Exception while typing attribute:@ %s" (Printexc.to_string exn)
      )
    | T.Superfluous_destructor _ -> Some (fun fmt () ->
        Format.fprintf fmt "Internal warning, please report upstream, ^^"
      )

    | T.Shadowing (id, old, _cur) -> Some (fun fmt () ->
        Format.fprintf fmt
          "Shadowing: %a was already %a"
          Dolmen.Std.Id.print id
          print_reason_opt (T.binding_reason old)
      )

    | Smtlib2_Ints.Restriction msg
    | Smtlib2_Reals.Restriction msg
    | Smtlib2_Reals_Ints.Restriction msg
      -> Some (fun fmt () ->
          Format.fprintf fmt
            "This is a non-linear expression according to the smtlib spec.%a"
            pp_hint msg
        )

    | Smtlib2_Float.Real_lit -> Some (fun fmt () ->
        Format.fprintf fmt
          "Real literals are not part of the Floats specification."
      )
    | Smtlib2_Float.Bitv_extended_lit -> Some (fun fmt () ->
        Format.fprintf fmt
          "Bitvector decimal literals are not part of the Floats specification."
      )


    | _ -> Some (fun fmt () ->
        Format.fprintf fmt
          "Unknown warning, please report upstream, ^^"
      )

  (* Report type errors *)
  (* ************************************************************************ *)

  let print_res fmt res =
    match (res : T.res) with
    | T.Ttype -> Format.fprintf fmt "Type"
    | T.Ty ty -> Format.fprintf fmt "the type@ %a" Dolmen.Std.Expr.Ty.print ty
    | T.Term t -> Format.fprintf fmt "the term@ %a" Dolmen.Std.Expr.Term.print t
    | T.Tags _ -> Format.fprintf fmt "some tags"

  let print_opt pp fmt = function
    | None -> Format.fprintf fmt "<none>"
    | Some x -> pp fmt x

  let rec print_expected fmt = function
    | [] -> assert false
    | x :: [] -> Format.fprintf fmt "%d" x
    | x :: r -> Format.fprintf fmt "%d or %a" x print_expected r

  let print_fragment (type a) fmt (fragment : a T.fragment) =
    match fragment with
    | T.Ast ast -> Dolmen.Std.Term.print fmt ast
    | T.Def d -> Dolmen.Std.Statement.print_def fmt d
    | T.Decl d -> Dolmen.Std.Statement.print_decl fmt d
    | T.Defs d ->
      Dolmen.Std.Statement.print_group Dolmen.Std.Statement.print_def fmt d
    | T.Decls d ->
      Dolmen.Std.Statement.print_group Dolmen.Std.Statement.print_decl fmt d
    | T.Located loc ->
      Format.fprintf fmt "<located at %a>" Dolmen.Std.ParseLocation.fmt loc

  let print_bt fmt bt =
    if Printexc.backtrace_status () then begin
      let s = Printexc.raw_backtrace_to_string bt in
      Format.fprintf fmt "@ @[<h>%a@]" Format.pp_print_text s
    end


  let report_error fmt (T.Error (_env, fragment, err)) =
    match err with

    (* Datatype definition not well founded *)
    | T.Not_well_founded_datatypes _ ->
      Format.fprintf fmt "Not well founded datatype declaration"

    (* Inference of the type of a bound variable *)
    | T.Infer_type_variable ->
      Format.fprintf fmt "Cannot infer the type of a variable"

    (* Generic error for when something was expected but not there *)
    | T.Expected (expect, got) ->
      Format.fprintf fmt "Expected %s but got %a"
        expect (print_opt print_res) got

    (* Arity errors *)
    | T.Bad_index_arity (s, expected, actual) ->
      Format.fprintf fmt
        "The indexed family of operators '%s' expects %d indexes, but was given %d"
        s expected actual
    | T.Bad_ty_arity (c, actual) ->
      Format.fprintf fmt "Bad arity: got %d arguments for type constant@ %a"
        actual Dolmen.Std.Expr.Print.ty_const c
    | T.Bad_op_arity (s, expected, actual) ->
      Format.fprintf fmt
        "Bad arity for operator '%s':@ expected %a arguments but got %d"
        s print_expected expected actual
    | T.Bad_cstr_arity (c, expected, actual) ->
      Format.fprintf fmt
        "Bad arity: expected %a arguments but got %d arguments for constructor@ %a%a"
        print_expected expected actual Dolmen.Std.Expr.Print.term_const c
        poly_hint (c, expected, actual)
    | T.Bad_term_arity (c, expected, actual) ->
      Format.fprintf fmt
        "Bad arity: expected %a but got %d arguments for function@ %a%a"
        print_expected expected actual Dolmen.Std.Expr.Print.term_const c
        poly_hint (c, expected, actual)

    (* Record constuction errors *)
    | T.Repeated_record_field f ->
      Format.fprintf fmt
        "The field %a is used more than once in this record construction"
        Dolmen.Std.Expr.Print.id f
    | T.Missing_record_field f ->
      Format.fprintf fmt
        "The field %a is missing from this record construction"
        Dolmen.Std.Expr.Print.id f
    | T.Mismatch_record_type (f, r) ->
      Format.fprintf fmt
        "The field %a does not belong to record type %a"
        Dolmen.Std.Expr.Print.id f Dolmen.Std.Expr.Print.id r

    (* Application of a variable *)
    | T.Var_application v ->
      Format.fprintf fmt "Cannot apply arguments to term variable@ %a" Dolmen.Std.Expr.Print.id v
    | T.Ty_var_application v ->
      Format.fprintf fmt "Cannot apply arguments to type variable@ %a" Dolmen.Std.Expr.Print.id v

    (* Wrong type *)
    | T.Type_mismatch (t, expected) ->
      Format.fprintf fmt "The term:@ %a@ has type@ %a@ but was expected to be of type@ %a"
        Dolmen.Std.Expr.Term.print t
        Dolmen.Std.Expr.Ty.print (Dolmen.Std.Expr.Term.ty t)
        Dolmen.Std.Expr.Ty.print expected

    | T.Quantified_var_inference ->
      Format.fprintf fmt "Cannot infer type for a quantified variable"

    | T.Unhandled_builtin b ->
      Format.fprintf fmt
        "The following Dolmen builtin is currently not handled@ %a.@ Please report upstream"
        Dolmen.Std.Term.print_builtin b

    | T.Cannot_tag_tag ->
      Format.fprintf fmt "Cannot apply a tag to another tag (only expressions)"

    | T.Cannot_tag_ttype ->
      Format.fprintf fmt "Cannot apply a tag to the Ttype constant"

    | T.Cannot_find id ->
      Format.fprintf fmt "Unbound identifier:@ '%a'" Dolmen.Std.Id.print id

    | T.Forbidden_quantifier ->
      Format.fprintf fmt "Quantified expressions are forbidden by the logic."

    | T.Type_var_in_type_constructor ->
      Format.fprintf fmt "Type variables cannot appear in the signature of a type constant"

    | T.Missing_destructor id ->
      Format.fprintf fmt
        "The destructor '%a'@ was not provided by the user implementation.@ Please report upstream."
        Dolmen.Std.Id.print id

    | T.Higher_order_application ->
      Format.fprintf fmt "Higher-order applications are not handled by the Tff typechecker"

    | T.Higher_order_type ->
      Format.fprintf fmt "Higher-order types are not handled by the Tff typechecker"

    | T.Unbound_variables (tys, [], _) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "The following variables are not bound:@ %a"
        (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Print.id) tys

    | T.Unbound_variables ([], ts, _) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "The following variables are not bound:@ %a"
        (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Print.id) ts

    | T.Unbound_variables (tys, ts, _) ->
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "The following variables are not bound:@ %a,@ %a"
        (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Print.id) tys
        (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Print.id) ts

    | T.Unhandled_ast ->
      Format.fprintf fmt
        "The typechecker did not know what to do with the following term.@ \
         Please report upstream.@\n%a"
        print_fragment fragment

    (* Tptp Arithmetic errors *)
    | Tptp_Arith.Expected_arith_type ty ->
      Format.fprintf fmt "Arithmetic type expected but got@ %a.@ %s"
        Dolmen.Std.Expr.Ty.print ty
        "Tptp arithmetic symbols are only polymorphic over the arithmetic types $int, $rat and $real."
    | Tptp_Arith.Cannot_apply_to ty ->
      Format.fprintf fmt "Cannot apply the arithmetic operation to type@ %a"
        Dolmen.Std.Expr.Ty.print ty

    (* Smtlib Arrya errors *)
    | Smtlib2_Arrays.Forbidden msg ->
      Format.fprintf fmt "Forbidden array sort.%a" pp_hint msg

    (* Smtlib Arithmetic errors *)
    | Smtlib2_Ints.Forbidden msg
    | Smtlib2_Reals.Forbidden msg
    | Smtlib2_Reals_Ints.Forbidden msg ->
      Format.fprintf fmt "Non-linear expressions are forbidden by the logic.%a" pp_hint msg
    | Smtlib2_Reals_Ints.Expected_arith_type ty ->
      Format.fprintf fmt "Arithmetic type expected but got@ %a.@ %s"
        Dolmen.Std.Expr.Ty.print ty
        "The stmlib Reals_Ints theory requires an arithmetic type in order to correctly desugar the expression."

    (* Smtlib Bitvector errors *)
    | Smtlib2_Bitv.Invalid_bin_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a binary bitvector litteral" c
    | Smtlib2_Bitv.Invalid_hex_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a hexadecimal bitvector litteral" c
    | Smtlib2_Bitv.Invalid_dec_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a decimal bitvector litteral" c

    (* Smtlib Float errors *)
    | Smtlib2_Float.Invalid_bin_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a binary bitvector litteral" c
    | Smtlib2_Float.Invalid_hex_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a hexadecimal bitvector litteral" c
    | Smtlib2_Float.Invalid_dec_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a decimal bitvector litteral" c

    (* Smtlib String errors *)
    | Smtlib2_String.Invalid_hexadecimal s ->
      Format.fprintf fmt "The following is not a valid hexadecimal character: '%s'" s
    | Smtlib2_String.Invalid_string_char c ->
      Format.fprintf fmt "The following character is not allowed in string literals: '%c'" c
    | Smtlib2_String.Invalid_escape_sequence (s, i) ->
      Format.fprintf fmt "The escape sequence starting at index %d in the \
                          following string is not allowed: '%s'" i s

    (* Expression filters *)
    | T.Uncaught_exn (Dolmen.Std.Expr.Filter_failed_ty (name, _ty, msg), _) ->
      Format.fprintf fmt "Filter '%s' failed for the given type.%a" name pp_hint msg
    | T.Uncaught_exn (Dolmen.Std.Expr.Filter_failed_term (name, _t, msg), _) ->
      Format.fprintf fmt "Filter '%s' failed for the given term.%a" name pp_hint msg

    (* Uncaught exception during type-checking *)
    | T.Uncaught_exn (exn, bt) ->
      Format.fprintf fmt
        "@[<v 2>Uncaught exception: %s%a@]"
        (Printexc.to_string exn) print_bt bt

    (* Warnings as errors *)
    | Warning_as_error w ->
      begin match report_warning w with
        | Some pp -> pp fmt ()
        | None ->
          Format.fprintf fmt "missing warning reporter, please report upstream, ^^"
      end

    (* Missing logic *)
    | Missing_logic ->
      Format.fprintf fmt "Missing logic (aka set-logic for smtlib2)."

    (* Illegal declarations *)
    | Illegal_decl ->
      Format.fprintf fmt "Illegal declaration. Hint: check your logic"

    (* Push/Pop errors *)
    | Invalid_push_n ->
      Format.fprintf fmt "Invalid push payload (payload must be positive)"
    | Invalid_pop_n ->
      Format.fprintf fmt "Invalid pop payload (payload must be positive)"
    | Pop_with_empty_stack ->
      Format.fprintf fmt "Pop instruction with an empty stack (likely a \
                          result of a missing push or excessive pop)"

    (* Catch-all *)
    | _ ->
      Format.fprintf fmt "Unknown typing error,@ please report upstream, ^^"

  let () =
    Printexc.register_printer (function
        | T.Typing_error error ->
          Some (Format.asprintf "Typing error:@ %a" report_error error)
        | _ -> None
      )

  (* Warning reporting and wrappers *)
  (* ************************************************************************ *)

  type warning_conf = {
    strict_typing : bool;
    smtlib2_6_shadow_rules : bool;
  }

  (* Warning reporter, sent to the typechecker.
     This is responsible for turning fatal warnings into errors *)
  let warnings_aux report conf ((T.Warning (env, fragment, warn)) as w) =
    match warn, fragment with
    (* Warnings as errors *)
    | T.Shadowing (_, (`Builtin `Term | `Not_found), `Variable _), fragment
    | T.Shadowing (_, (`Constant _ | `Builtin _ | `Not_found), `Constant _), fragment
      when conf.smtlib2_6_shadow_rules ->
      T._error env fragment (Warning_as_error w)

    | Smtlib2_Ints.Restriction _, fragment
    | Smtlib2_Reals.Restriction _, fragment
    | Smtlib2_Reals_Ints.Restriction _, fragment
      when conf.strict_typing ->
      T._error env fragment (Warning_as_error w)

    | Smtlib2_Float.Real_lit, fragment
    | Smtlib2_Float.Bitv_extended_lit, fragment
      when conf.strict_typing ->
      T._error env fragment (Warning_as_error w)

    (* general case *)
    | _ -> report w

  (* Generate typing env from state *)
  (* ************************************************************************ *)

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

  let typing_env ?(loc=no_loc) warnings (st : S.t) =

    let additional_builtins env args =
      !additional_builtins env args
    in

    (* Match the language to determine bultins and other options *)
    match (S.input_lang st : Parser.language option) with
    | None -> assert false

    (* Dimacs & iCNF
       - these infer the declarations of their constants
         (we could declare them when the number of clauses and variables
         is declared in the file, but it's easier this way).
       - there are no explicit declaration or definitions, hence no builtins *)
    | Some Dimacs | Some ICNF ->
      let poly = T.Flexible in
      let expect = T.Typed Dolmen.Std.Expr.Ty.prop in
      let infer_base = Some Dolmen.Std.Expr.Ty.prop in
      let warnings = warnings {
          strict_typing = S.strict_typing st;
          smtlib2_6_shadow_rules = false;
        } in
      let builtins = Dolmen_type.Base.noop in
      T.empty_env
        ~st:(S.ty_state st).typer
        ~expect ?infer_base ~poly
        ~warnings builtins

    (* Alt-Ergo format
    *)
    | Some Alt_ergo ->
      let poly = T.Flexible in
      let expect = T.Nothing in
      let infer_base = None in
      let warnings = warnings {
          strict_typing = S.strict_typing st;
          smtlib2_6_shadow_rules = false;
        } in
      let builtins = Dolmen_type.Base.merge [
          Ae_Core.parse;
          Decl.parse; Subst.parse;
          additional_builtins
        ] in
      T.empty_env
        ~st:(S.ty_state st).typer
        ~expect ?infer_base ~poly
        ~warnings builtins

    (* Zipperposition Format
       - no inference of constants
       - only the base builtin
    *)
    | Some Zf ->
      let poly = T.Flexible in
      let expect = T.Nothing in
      let infer_base = None in
      let warnings = warnings {
          strict_typing = S.strict_typing st;
          smtlib2_6_shadow_rules = false;
        } in
      let builtins = Dolmen_type.Base.merge [
          Decl.parse;
          Subst.parse;
          additional_builtins;
          Zf_Core.parse;
        ] in
      T.empty_env
        ~st:(S.ty_state st).typer
        ~expect ?infer_base ~poly
        ~warnings builtins

    (* TPTP
       - tptp has inference of constants
       - 2 base theories (Core and Arith) + the builtin Decl and Subst
         for explicit declaration and definitions
    *)
    | Some Tptp v ->
      let poly = T.Explicit in
      let expect = T.Typed Dolmen.Std.Expr.Ty.prop in
      let infer_base = Some Dolmen.Std.Expr.Ty.base in
      let warnings = warnings {
          strict_typing = S.strict_typing st;
          smtlib2_6_shadow_rules = false;
        } in
      let builtins = Dolmen_type.Base.merge [
          Decl.parse;
          Subst.parse;
          additional_builtins;
          Tptp_Core.parse v;
          Tptp_Arith.parse v;
        ] in
      T.empty_env
        ~st:(S.ty_state st).typer
        ~expect ?infer_base ~poly
        ~warnings builtins

    (* SMTLib v2
       - no inference
       - see the dedicated function for the builtins
       - restrictions come from the logic declaration
       - shadowing is forbidden
    *)
    | Some Smtlib2 v ->
      let poly = T.Implicit in
      let expect = T.Nothing in
      let infer_base = None in
      let warnings = warnings {
          strict_typing = S.strict_typing st;
          smtlib2_6_shadow_rules = match v with
            | `Latest | `V2_6 -> true;
        } in
      begin match (S.ty_state st).logic with
        | Auto ->
          let builtins = Dolmen_type.Base.noop in
          let env = T.empty_env
              ~st:(S.ty_state st).typer ~poly ~expect ~warnings builtins
          in
          T._error env (Located loc) Missing_logic
        | Smtlib2 logic ->
          let builtins = Dolmen_type.Base.merge (
              Decl.parse :: Subst.parse :: additional_builtins ::
              builtins_of_smtlib2_logic v logic
            ) in
          let quants = logic.features.quantifiers in
          T.empty_env
            ~st:(S.ty_state st).typer
            ~expect ?infer_base ~poly ~quants
            ~warnings builtins
      end

  let typing_wrap ?loc st ~f =
    let st = ref st in
    let report (T.Warning (_, fg, _) as w) =
      let loc = T.fragment_loc fg in
      match report_warning w with
      | None -> ()
      | Some pp -> st := S.warn ?loc !st "%a" pp ()
    in
    let env = typing_env ?loc (warnings_aux report) !st in
    let res = f env in
    !st, res

  (* Push&Pop *)
  (* ************************************************************************ *)

  let reset st ?loc:_ () =
    S.set_ty_state st (new_state ())

  let rec push st ?(loc=no_loc) = function
    | 0 -> st
    | i ->
      if i <= 0 then
        let env = typing_env ~loc (fun _ _ -> ()) st in
        T._error env (Located loc) Invalid_push_n
      else begin
        let t = S.ty_state st in
        let st' = T.copy_state t.typer in
        let t' = { t with stack = st' :: t.stack; } in
        let st' = S.set_ty_state st t' in
        push st' (i - 1)
      end

  let rec pop st ?(loc=no_loc) = function
    | 0 -> st
    | i ->
      if i <= 0 then
        let env = typing_env ~loc (fun _ _ -> ()) st in
        T._error env (Located loc) Invalid_pop_n
      else begin
        let t = S.ty_state st in
        match t.stack with
        | [] ->
          let env = typing_env ~loc (fun _ _ -> ()) st in
          T._error env (Located loc) Pop_with_empty_stack
        | ty :: r ->
          let t' = { t with typer = ty; stack = r; } in
          let st' = S.set_ty_state st t' in
          pop st' (i - 1)
      end


  (* Setting the logic *)
  (* ************************************************************************ *)

  let set_logic (st : S.t) ?loc s =
    match (S.input_lang st : Parser.language option) with
    | Some Dimacs -> st
    | Some Smtlib2 _ ->
      let st, l =
        match Dolmen_type.Logic.Smtlib2.parse s with
        | Some l -> st, l
        | None ->
          let st = S.warn ?loc st "Unknown logic %s" s in
          st, Dolmen_type.Logic.Smtlib2.all
      in
      S.set_ty_state st { (S.ty_state st) with logic = Smtlib2 l; }
    | _ ->
      S.warn ?loc st
        "Set logic is not supported for the current language"


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
    | `Type_decl (c : Dolmen.Std.Expr.ty_const) ->
      begin match Dolmen.Std.Expr.Ty.definition c with
        | None | Some Abstract ->
          if not (allow_abstract_type_decl st) then
            T._error env (Decl d) Illegal_decl
        | Some Adt _ ->
          if not (allow_data_type_decl st) then
            T._error env (Decl d) Illegal_decl
      end
    | `Term_decl (c : Dolmen.Std.Expr.term_const) ->
      let is_function = c.ty.fun_vars <> [] || c.ty.fun_args <> [] in
      if is_function && not (allow_function_decl st) then
        T._error env (Decl d) Illegal_decl

  let check_decls st env l decls =
    List.iter2 (check_decl st env) l decls

  let decls (st : S.t) ?loc ?attr d =
    typing_wrap ?loc st ~f:(fun env ->
        let decls = T.decls env ?attr d in
        let () = check_decls st env d.contents decls in
        decls
      )


  (* Definitions *)
  (* ************************************************************************ *)

  let defs st ?loc ?attr d =
    typing_wrap ?loc st ~f:(fun env ->
        let l = T.defs env ?attr d in
        let l = List.map (function
            | `Type_def (id, _, vars, body) ->
              let () = if not d.recursive then Subst.define_ty id vars body in
              `Type_def (id, vars, body)
            | `Term_def (id, f, vars, args, body) ->
              let () = Decl.add_definition id (`Term f) in
              `Term_def (id, f, vars, args, body)
          ) l
        in
        l
      )

  (* Wrappers around the Type-checking module *)
  (* ************************************************************************ *)

  let typecheck = S.typecheck

  let terms st ?loc ?attr:_ l =
    typing_wrap ?loc st ~f:(fun env ->
        List.map (T.parse_term env) l
      )

  let formula st ?loc ?attr:_ ~goal:_ (t : Dolmen.Std.Term.t) =
    typing_wrap ?loc st ~f:(fun env ->
        T.parse env t
      )

  let formulas st ?loc ?attr:_ l =
    typing_wrap ?loc st ~f:(fun env ->
        List.map (T.parse env) l
      )

end

