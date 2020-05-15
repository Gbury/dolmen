
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = sig

  type type_st
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
    with type state := (Parser.language, type_st, solve_st) Dolmen.State.state
     and type ty := Dolmen.Expr.ty
     and type ty_var := Dolmen.Expr.ty_var
     and type ty_const := Dolmen.Expr.ty_const
     and type term := Dolmen.Expr.term
     and type term_var := Dolmen.Expr.term_var
     and type term_const := Dolmen.Expr.term_const
     and type formula := Dolmen.Expr.formula

  val new_state : unit -> type_st

  val report_error : Format.formatter -> T.error -> unit
  val report_warning : T.warning -> (Format.formatter -> unit -> unit) option

end

module Make(S : State_intf.Typer) = struct

  (* Dolmen_type functors instantiation *)
  (* ************************************************************************ *)

  module T = Dolmen_type.Tff.Make
      (Dolmen.Tag)(Dolmen.Expr.Ty)(Dolmen.Expr.Term)

  (* Definitions builtin *)
  module Decl = Dolmen_type.Def.Declare(T)
  module Subst = Dolmen_type.Def.Subst(T)(struct

      let of_list l =
        let aux acc (k, v) = Dolmen.Expr.Subst.Var.bind acc k v in
        List.fold_left aux Dolmen.Expr.Subst.empty l

      let ty_subst l ty =
        Dolmen.Expr.Ty.subst (of_list l) ty

      let term_subst tys terms t =
        Dolmen.Expr.Term.subst (of_list tys) (of_list terms) t
    end)

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
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term.Bitv)
  module Smtlib2_Float =
    Dolmen_type.Float.Smtlib2.Tff(T)
      (Dolmen.Expr.Ty)(Dolmen.Expr.Term)

  (* Zf *)
  module Zf_Base =
    Dolmen_type.Base.Zf.Tff(T)(Dolmen.Expr.Tags)

  (* Typing state *)
  (* ************************************************************************ *)

  (* This is here to define the typing state (not ot confuse with the state
     passed in the pipes, which will contain the typing state. *)

  type type_st = {
    (* typechecker global state *)
    typer : T.state;
    (* logic used *)
    logic : Dolmen_type.Logic.t;
  }

  let new_state () = {
    typer = T.new_state ();
    logic = Auto;
  }

  (* New warnings & errors *)
  (* ************************************************************************ *)

  let no_loc = Dolmen.ParseLocation.mk "" 0 0 0 0

  type _ T.err +=
    | Warning_as_error : T.warning -> _ T.err
    | Missing_logic : Dolmen.ParseLocation.t T.err
    | Illegal_decl : Dolmen.Statement.decl T.err

  (* Report type warnings *)
  (* ************************************************************************ *)

  let pp_opt_loc fmt = function
    | None -> Format.fprintf fmt "<location missing>"
    | Some loc -> Dolmen.ParseLocation.fmt_pos fmt loc

  let decl_loc d =
    match (d : Dolmen.Statement.decl) with
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
    | Declared d ->
      Format.fprintf fmt "declared at %a" pp_opt_loc (decl_loc d)

  let report_warning (T.Warning (_env, _fragment, warn)) =
    match warn with
    | T.Unused_type_variable v -> Some (fun fmt () ->
        Format.fprintf fmt
          "Quantified type variable `%a` is unused"
          Dolmen.Expr.Print.ty_var v
      )
    | T.Unused_term_variable v -> Some (fun fmt () ->
        Format.fprintf fmt
          "Quantified term variable `%a` is unused"
          Dolmen.Expr.Print.term_var v
      )
    | T.Error_in_attribute exn -> Some (fun fmt () ->
        Format.fprintf fmt
          "Error while typing attribute:@ %s" (Printexc.to_string exn)
      )
    | T.Superfluous_destructor _ -> None

    | T.Shadowing (id, old, _cur) -> Some (fun fmt () ->
        Format.fprintf fmt
          "Shadowing: %a was already %a"
          Dolmen.Id.print id print_reason (T.binding_reason old)
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
    | T.Ty ty -> Format.fprintf fmt "the type@ %a" Dolmen.Expr.Ty.print ty
    | T.Term t -> Format.fprintf fmt "the term@ %a" Dolmen.Expr.Term.print t
    | T.Tags _ -> Format.fprintf fmt "some tags"

  let print_opt pp fmt = function
    | None -> Format.fprintf fmt "<none>"
    | Some x -> pp fmt x

  let rec print_expected fmt = function
    | [] -> assert false
    | x :: [] -> Format.fprintf fmt "%d" x
    | x :: r -> Format.fprintf fmt "%d or %a" x print_expected r

  let report_error fmt (T.Error (_env, _fragment, err)) =
    match err with
    (* Core Typechecking Errors *)
    | T.Infer_type_variable ->
      Format.fprintf fmt "Cannot infer the type of a variable"
    | T.Expected (expect, got) ->
      Format.fprintf fmt "Expected %s but got %a"
        expect (print_opt print_res) got
    | T.Bad_op_arity (s, expected, actual) ->
      Format.fprintf fmt
        "Bad arity for builtin '%s':@ expected %a arguments but got %d"
        s print_expected expected actual
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
        "Tptp arithmetic symbols are only polymorphic over the arithmetic types $int, $rat and $real."
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

    (* Smtlib Float errors *)
    | Smtlib2_Float.Invalid_bin_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a binary bitvector litteral" c
    | Smtlib2_Float.Invalid_hex_char c ->
      Format.fprintf fmt "The character '%c' is invalid inside a hexadecimal bitvector litteral" c

    (* Linear arithmetic *)
    | T.Uncaught_exn (Dolmen.Expr.Filter_failed_term (name, _t))
      when name = Dolmen.Expr.Filter.Linear.name ->
      Format.fprintf fmt "Non-linear expressions are forbidden by the logic."
    (* Quantifier free formulas *)
    | T.Uncaught_exn (Dolmen.Expr.Filter_failed_term (name, _t))
      when name = Dolmen.Expr.Filter.Quantifier.name ->
      Format.fprintf fmt "Quantified expressions are forbidden by the logic."

    (* Expression filters *)
    | T.Uncaught_exn (Dolmen.Expr.Filter_failed_ty (name, _ty)) ->
      Format.fprintf fmt "Filter '%s' failed for the given type." name
    | T.Uncaught_exn (Dolmen.Expr.Filter_failed_term (name, _t)) ->
      Format.fprintf fmt "Filter '%s' failed for the given term." name

    (* Uncaught exception during type-checking *)
    | T.Uncaught_exn exn ->
      Format.fprintf fmt "Uncaught exception: %s" (Printexc.to_string exn)

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

    (* Catch-all *)
    | _ ->
      Format.fprintf fmt "Unknown typing error,@ please report upstream, ^^"

  let () =
    Printexc.register_printer (function
        | T.Typing_error error ->
          Some (Format.asprintf "Typing error:@ %a" report_error error)
        | _ -> None
      )

  (* Warning reporting *)
  (* ************************************************************************ *)

  (* Transform a warning into an option loc and message *)
  let warning_to_loc_msg (T.Warning (_, fg, _) as w) =
    let loc =
      match T.fragment_loc fg with
      | Some l -> l
      | None -> no_loc
    in
    match report_warning w with
    | None -> None
    | Some pp ->
      let msg = Format.asprintf "%a" pp () in
      Some (loc, msg)

  let add_warning, add_raw_warning, get_warnings =
    (* List ref to store a list of warnings *)
    let l = ref [] in
    (* Directly add a loc and message *)
    let add_raw loc msg = l := (loc, msg) :: !l in
    (* Add a warning *)
    let add w =
      match warning_to_loc_msg w with
      | None -> ()
      | Some (loc, msg) -> add_raw loc msg
    in
    (* Get back the list of warnings as a list loc * message *)
    let get () =
      let res = List.rev !l in
      l := [];
      res
    in
    add, add_raw, get

  type warning_conf = {
    error_on_shadow : bool;
  }

  (* Warning reporter, sent to the typechecker.
     This is responsible for turning fatal warnings into errors *)
  let warnings conf ((T.Warning (env, fragment, warn)) as w) =
    match warn, fragment with
    (* Warnings as errors *)
    | T.Shadowing (_, _, `Constant _), fragment
      when conf.error_on_shadow ->
      T._error env fragment (Warning_as_error w)

    (* general case *)
    | _ -> add_warning w



  (* Generate typing env from state *)
  (* ************************************************************************ *)

  let builtins_of_smtlib2_logic v l =
    List.fold_left (fun acc th ->
        match (th : Dolmen_type.Logic.Smtlib2.theory) with
        | `Core -> Smtlib2_Base.parse v :: acc
        | `Ints -> Smtlib2_Ints.parse v :: acc
        | `Arrays -> Smtlib2_Arrays.parse v :: acc
        | `Bitvectors -> Smtlib2_Bitv.parse v :: acc
        | `Floats -> Smtlib2_Float.parse v :: acc
        | `Reals -> Smtlib2_Reals.parse v :: acc
        | `Reals_Ints -> Smtlib2_Reals_Ints.parse v :: acc
      ) [] l.Dolmen_type.Logic.Smtlib2.theories

  let reset_restrictions () =
    Dolmen.Expr.Filter.reset ();
    ()

  let restrictions_of_smtlib2_logic _v (l: Dolmen_type.Logic.Smtlib2.t) =
    (* Arithmetic restrictions *)
    begin match l.features.arithmetic with
      | `Regular -> ()
      | `Linear -> Dolmen.Expr.Filter.Linear.active := true
      | `Difference -> (* TODO *) ()
    end;
    (* Quantifiers restrictions *)
    Dolmen.Expr.Filter.Quantifier.allow := l.features.quantifiers;
    ()

  let typing_env
      ?(loc=no_loc)
      (st : (Parser.language, type_st, _) Dolmen.State.state) =
    (* Reset restrictions (useful if multiple instances of the typing
       functions are used to type different inputs conccurrently *)
    reset_restrictions ();


    (* Match the language to determine bultins and other options *)
    match st.input_lang with
    | None -> assert false

    (* Dimacs & iCNF
       - these infer the declarations of their constants
         (we could declare them when the number of clauses and variables
         is declared in the file, but it's easier this way).
       - there are no explicit declaration or definitions, hence no builtins *)
    | Some Dimacs | Some ICNF ->
      let expect = T.Typed Dolmen.Expr.Ty.prop in
      let infer_base = Some Dolmen.Expr.Ty.prop in
      let warnings = warnings {
          error_on_shadow = false;
        } in
      let builtins = Dolmen_type.Base.noop in
      T.empty_env
        ~st:st.type_state.typer
        ~expect ?infer_base
        ~warnings builtins

    (* Alt-Ergo format
       *)
    | Some Alt_ergo ->
      let expect = T.Nothing in
      let infer_base = None in
      let warnings = warnings {
          error_on_shadow = false;
        } in
      let builtins = Dolmen_type.Base.merge [
          Decl.parse; Subst.parse;
        ] in
      T.empty_env
        ~st:st.type_state.typer
        ~expect ?infer_base
        ~warnings builtins

    (* Zipperposition Format
       - no inference of constants
       - only the base builtin
    *)
    | Some Zf ->
      let expect = T.Nothing in
      let infer_base = None in
      let warnings = warnings {
          error_on_shadow = false;
        } in
      let builtins = Dolmen_type.Base.merge [
          Decl.parse;
          Subst.parse;
          Zf_Base.parse;
        ] in
      T.empty_env
        ~st:st.type_state.typer
        ~expect ?infer_base
        ~warnings builtins

    (* TPTP
       - tptp has inference of constants
       - 2 base theories (Base and Arith) + the builtin Decl and Subst
         for explicit declaration and definitions
       *)
    | Some Tptp v ->
      let expect = T.Typed Dolmen.Expr.Ty.prop in
      let infer_base = Some Dolmen.Expr.Ty.base in
      let warnings = warnings {
          error_on_shadow = false;
        } in
      let builtins = Dolmen_type.Base.merge [
          Decl.parse;
          Subst.parse;
          Tptp_Base.parse v;
          Tptp_Arith.parse v;
        ] in
      T.empty_env
        ~st:st.type_state.typer
        ~expect ?infer_base
        ~warnings builtins

    (* SMTLib v2
       - no inference
       - see the dedicated function for the builtins
       - restrictions come from the logic declaration
       - shadowing is forbidden
       *)
    | Some Smtlib2 v ->
      let expect = T.Nothing in
      let infer_base = None in
      let warnings = warnings {
          error_on_shadow = true;
        } in
      begin match st.type_state.logic with
        | Auto ->
          let builtins = Dolmen_type.Base.noop in
          let env = T.empty_env
              ~st:st.type_state.typer ~expect ~warnings builtins
          in
          T._error env (Located loc) Missing_logic
        | Smtlib2 logic ->
          let builtins = Dolmen_type.Base.merge (
              Decl.parse :: Subst.parse ::
              builtins_of_smtlib2_logic v logic
            ) in
          let () = restrictions_of_smtlib2_logic v logic in
          T.empty_env
            ~st:st.type_state.typer
            ~expect ?infer_base
            ~warnings builtins
      end

  (* Setting the logic *)
  (* ************************************************************************ *)

  let default_smtlib2_logic = {
    Dolmen_type.Logic.Smtlib2.theories =
      [ `Core; `Reals_Ints; `Arrays; `Bitvectors; ];
    features = {
      uninterpreted = true;
      datatypes = true;
      quantifiers = true;
      arithmetic = `Regular;
    };
  }

  let set_logic (st : _ Dolmen.State.state) ?(loc=no_loc) s =
    (* auxiliary funciton/lens to set the logic in the state *)
    let set (st : _ Dolmen.State.state) logic = {
      st with type_state = {
        st.type_state with logic;
      };
    } in
    (* *)
    match (st.input_lang : Parser.language option) with
    | Some Smtlib2 _ ->
      let l =
        match Dolmen_type.Logic.Smtlib2.parse s with
        | Some l -> l
        | None ->
          add_raw_warning loc (Format.asprintf "Unknown logic %s" s);
          default_smtlib2_logic
      in
      set st (Smtlib2 l)
    | _ ->
      add_raw_warning loc (
        Format.asprintf "Set logic is not supported for the current language"
      );
      st



  (* Declarations *)
  (* ************************************************************************ *)

  let allow_function_decl (st : _ Dolmen.State.state) =
    match st.type_state.logic with
      | Smtlib2 logic -> logic.features.uninterpreted
      | Auto -> true

  let allow_data_type_decl (st : _ Dolmen.State.state) =
    match st.type_state.logic with
      | Smtlib2 logic -> logic.features.datatypes
      | Auto -> true

  let allow_abstract_type_decl (st : _ Dolmen.State.state) =
    match st.type_state.logic with
      | Smtlib2 logic -> logic.features.uninterpreted
      | Auto -> true

  let check_decl st env d = function
    | `Type_decl (c : Dolmen.Expr.ty_const) ->
      begin match Dolmen.Expr.Ty.definition c with
        | None | Some Abstract ->
          if not (allow_abstract_type_decl st) then
            T._error env (Decl d) Illegal_decl
        | Some Adt _ ->
          if not (allow_data_type_decl st) then
            T._error env (Decl d) Illegal_decl
      end
    | `Term_decl (c : Dolmen.Expr.term_const) ->
      let is_function = c.ty.fun_vars <> [] || c.ty.fun_args <> [] in
      if is_function && not (allow_function_decl st) then
        T._error env (Decl d) Illegal_decl

  let check_decls st env l decls =
    List.iter2 (check_decl st env) l decls

  let decls st ?attr l =
    let env = typing_env st in
    let decls = T.decls env ?attr l in
    let () = check_decls st env l decls in
    st, decls, get_warnings ()


  (* Wrappers around the Type-checking module *)
  (* ************************************************************************ *)

  let typecheck (st : _ Dolmen.State.state) = st.type_check

  let def st ?attr id (t : Dolmen.Term.t) =
    let env = typing_env ?loc:t.loc st in
    begin match T.new_def ?attr env t id with
      | `Type_def (id, _, vars, body) ->
        let () = Subst.define_ty id vars body in
        st, `Type_def (id, vars, body), get_warnings ()
      | `Term_def (id, _, vars, args, body) ->
        let expr_id = Decl.define_term id vars args body in
        st, `Term_def (id, expr_id, vars, args, body), get_warnings ()
    end

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

