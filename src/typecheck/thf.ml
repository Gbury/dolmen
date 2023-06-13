
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = Thf_intf.S

(* Typechecking functor *)
(* ************************************************************************ *)

module Make
    (Tag: Dolmen.Intf.Tag.S)
    (Ty: Dolmen.Intf.Ty.Thf
     with type 'a tag := 'a Tag.t
      and type path := Dolmen.Std.Path.t)
    (T: Dolmen.Intf.Term.Thf
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type ty_def := Ty.def
      and type 'a tag := 'a Tag.t
      and type path := Dolmen.Std.Path.t)
= struct

  (* Module aliases *)
  (* ************************************************************************ *)

  (* These are exported *)
  module T = T
  module Ty = Ty
  module Tag = Tag

  (* Non-exported module alias to avoid confusing
     untyped Terms and typed terms *)
  module Id = Dolmen.Std.Id
  module Ast = Dolmen.Std.Term
  module Stmt = Dolmen.Std.Statement
  module Loc = Dolmen.Std.Loc

  (* Custom alias for convenience *)
  module Hmap = Dolmen.Std.Tag

  (* Types *)
  (* ************************************************************************ *)

  (* FO vs HO *)
  type order =
    | First_order
    | Higher_order

  (* Different behavior of polymorphism *)
  type poly =
    | Explicit
    | Implicit
    | Flexible

  (* The source of a wildcard. *)
  type sym_inference_source = {
    symbol : Id.t;
    symbol_loc : Loc.t;
    mutable inferred_ty : Ty.t;
  }

  type var_inference_source = {
    variable : Id.t;
    variable_loc : Loc.t;
    mutable inferred_ty : Ty.t;
  }

  type wildcard_source =
    | Arg_of of wildcard_source
    | Ret_of of wildcard_source
    | From_source of Ast.t
    | Added_type_argument of Ast.t
    | Symbol_inference of sym_inference_source
    | Variable_inference of var_inference_source

  type wildcard_shape =
    | Forbidden
    | Any_in_scope
    | Any_base of {
        allowed : Ty.t list;
        preferred : Ty.t;
      }
    | Arrow of {
        arg_shape : wildcard_shape;
        ret_shape : wildcard_shape;
      }

  type infer_unbound_var_scheme =
    | No_inference (* the only sane default *)
    | Unification_type_variable

  type infer_term_scheme =
    | No_inference
    | Wildcard of wildcard_shape

  type free_wildcards =
    | Forbidden (* the reasonable default *)
    | Implicitly_universally_quantified

  type expect =
    | Type
    | Term
    | Anything

  (* The type returned after parsing an expression. *)
  type tag =
    | Set : 'a Tag.t * 'a -> tag
    | Add : 'a list Tag.t * 'a -> tag

  (* Result of parsing an expression *)
  type res =
    | Ttype
    | Ty    of Ty.t
    | Term  of T.t
    | Tags  of tag list


  (* Wrapper around potential function symbols in Dolmen *)
  type symbol = Intf.symbol =
    | Id of Id.t
    | Builtin of Ast.builtin

  (* Not found result *)
  type not_found = [ `Not_found ]

  (* Variable that can be bound to a dolmen identifier *)
  type ty_var = [
    | `Ty_var of Ty.Var.t
  ]

  type term_var = [
    | `Term_var of T.Var.t
  ]

  type 'env let_var = [
    | `Letin of 'env * Ast.t * T.Var.t * T.t
  ]

  type 'env bound_var = [
    | ty_var | term_var | 'env let_var
  ]

  (* various contants *)
  type ty_cst = [
    | `Ty_cst of Ty.Const.t
  ]

  type term_cst = [
    | `Term_cst of T.Const.t
  ]

  type term_cstr = [
    | `Cstr of T.Cstr.t
  ]

  type term_dstr = [
    | `Dstr of T.Const.t
  ]

  type term_field = [
    | `Field of T.Field.t
  ]

  (* Constants that can be bound to a dolmen identifier. *)
  type cst = [ ty_cst | term_cst | term_cstr | term_dstr | term_field ]

  (* Inference of variables and symbols *)
  type var_infer = {
    infer_unbound_vars              : infer_unbound_var_scheme;
    infer_type_vars_in_binding_pos  : bool;
    infer_term_vars_in_binding_pos  : infer_term_scheme;
    var_hook : [ ty_var | term_var ] -> unit;
  }

  type sym_infer = {
    infer_type_csts   : bool;
    infer_term_csts   : infer_term_scheme;
    sym_hook : [ ty_cst | term_cst ] -> unit;
  }

  (* Record for results  *)
  type ('res, 'meta) builtin_common_res =
    'meta * (Ast.t -> Ast.t list -> 'res)

  (* term semantics *)
  type term_semantics = [
    | `Total
    | `Partial of (Ty.Var.t list -> T.Var.t list -> Ty.t -> T.Const.t)
  ]

  (* builtin meta types *)
  type builtin_meta_ttype = unit
  type builtin_meta_ty = unit
  type builtin_meta_tags = unit
  type builtin_meta_term = term_semantics

  (* Result of parsing a symbol by the theory *)
  type builtin_common = [
    | `Ttype of (unit, builtin_meta_ttype) builtin_common_res
    | `Ty    of (Ty.t, builtin_meta_ty) builtin_common_res
    | `Term  of (T.t, builtin_meta_term) builtin_common_res
    | `Tags  of (tag list, builtin_meta_tags) builtin_common_res
  ]

  type builtin_infer = [
    | `Infer of string * var_infer * sym_infer
  ]

  type builtin_reserved = [
    | `Reserved of string * [
        | `Solver
        | `Term_cst of (Ty.Var.t list -> T.Var.t list -> Ty.t -> T.Const.t)
      ]
  ]

  type builtin_res = [ builtin_common | builtin_infer | builtin_reserved ]

  (* Names that are bound to a dolmen identifier by the builtins *)
  type builtin = [
    | `Builtin of builtin_res
  ]

  type reason =
    | Builtin
    | Reserved of string
    | Bound of Loc.file * Ast.t
    | Inferred of Loc.file * Ast.t
    | Defined of Loc.file * Stmt.def
    | Declared of Loc.file * Stmt.decl
    | Implicit_in_def of Loc.file * Stmt.def
    | Implicit_in_decl of Loc.file * Stmt.decl
    | Implicit_in_term of Loc.file * Ast.t
  (** The type of reasons for constant typing *)

  type binding = [
    | `Not_found
    | `Reserved of string
    | `Builtin of [
        | `Ttype
        | `Ty
        | `Term
        | `Tag
      ]
    | `Variable of [
        | `Ty of Ty.Var.t * reason option
        | `Term of T.Var.t * reason option
      ]
    | `Constant of [
        | `Ty of Ty.Const.t * reason option
        | `Cstr of T.Cstr.t * reason option
        | `Dstr of T.Const.t * reason option
        | `Term of T.Const.t * reason option
        | `Field of T.Field.t * reason option
      ]
  ]
  (** The bindings that can occur. *)

  type var_kind = [
    | `Let_bound
    | `Quantified
    | `Function_param
    | `Type_alias_param
  ]
  (** The type of kinds of variables *)

  (* Maps & Hashtbls *)
  (* ************************************************************************ *)

  module M = Id.Map

  module E = Map.Make(Ty.Var)
  module F = Map.Make(T.Var)
  module R = Map.Make(Ty.Const)
  module S = Map.Make(T.Const)
  module U = Map.Make(T.Cstr)
  module V = Map.Make(T.Field)


  (* Warnings & Errors *)
  (* ************************************************************************ *)

  (* Fragments of input that represent the sources of warnings/errors *)
  type _ fragment =
    | Ast : Ast.t -> Ast.t fragment
    | Def : Stmt.def -> Stmt.def fragment
    | Defs : Stmt.defs -> Stmt.defs fragment
    | Decl : Stmt.decl -> Stmt.decl fragment
    | Decls : Stmt.decls -> Stmt.decls fragment
    | Located : Loc.t -> Loc.t fragment

  let decl_loc d =
    match (d : Stmt.decl) with
    | Record { loc; _ }
    | Abstract { loc; _ }
    | Inductive { loc; _ } -> loc

  (* Warnings *)
  (* ******** *)

  (* Warnings, parameterized by the type of fragment they can trigger on *)
  type _ warn = ..

  type _ warn +=
    | Unused_type_variable : var_kind * Ty.Var.t -> Ast.t warn
    (* Unused bound type variable *)
    | Unused_term_variable : var_kind * T.Var.t -> Ast.t warn
    (* Unused bound term variable *)
    | Error_in_attribute : exn -> Ast.t warn
    (* An error occurred wile parsing an attribute *)
    | Superfluous_destructor : Id.t * Id.t * T.Const.t -> Ast.t warn
    (* The user implementation of typed terms returned a destructor where
       was asked for. This warning can very safely be ignored. *)
    | Redundant_pattern : T.t -> Ast.t warn
    (* Redundant cases in pattern matching *)

  (* Special case for shadowing, as it can happen both from a term but also
     a declaration, hence why the type variable of [warn] is left wild. *)
  type _ warn +=
    | Shadowing : Id.t * binding * binding -> _ warn
    (* Shadowing of the given identifier, together with the old and current
       binding. *)


  (* Errors *)
  (* ****** *)

  (* Errors, parameterized by the kind of fragment they can trigger on *)
  type _ err = ..

  (* Errors that occur on declaration(s) *)
  type _ err +=
    | Not_well_founded_datatypes : Stmt.decl list -> Stmt.decls err
    (* Not well-dounded datatypes definitions. *)

  (* Errors that occur on term fragments, i.e. Ast.t fragments *)
  type _ err +=
    | Expected : string * res option -> Ast.t err
    | Bad_index_arity : string * int * int -> Ast.t err
    | Bad_ty_arity : Ty.Const.t * int -> Ast.t err
    | Bad_op_arity : symbol * int list * int -> Ast.t err
    | Bad_cstr_arity : T.Cstr.t * int list * int -> Ast.t err
    | Bad_term_arity : T.Const.t * int list * int -> Ast.t err
    | Bad_poly_arity : Ty.Var.t list * Ty.t list -> Ast.t err
    | Over_application : T.t list -> Ast.t err
    | Repeated_record_field : T.Field.t -> Ast.t err
    | Missing_record_field : T.Field.t -> Ast.t err
    | Mismatch_record_type : T.Field.t * Ty.Const.t -> Ast.t err
    | Mismatch_sum_type : T.Cstr.t * Ty.t -> Ast.t err
    | Partial_pattern_match : T.t list -> Ast.t err
    | Var_application : T.Var.t -> Ast.t err
    | Ty_var_application : Ty.Var.t -> Ast.t err
    | Type_mismatch : T.t * Ty.t -> Ast.t err
    | Var_in_binding_pos_underspecified : Ast.t err
    | Unhandled_builtin : Ast.builtin -> Ast.t err
    | Cannot_tag_tag : Ast.t err
    | Cannot_tag_ttype : Ast.t err
    | Cannot_find : Id.t * string -> Ast.t err
    | Forbidden_quantifier : Ast.t err
    | Missing_destructor : Id.t -> Ast.t err
    | Type_def_rec : Stmt.def -> Stmt.defs err
    | Id_definition_conflict : Id.t * binding -> Loc.t err
    | Higher_order_application : Ast.t err
    | Higher_order_type : Ast.t err
    | Higher_order_env_in_tff_typechecker : Loc.t err
    | Polymorphic_function_argument : Ast.t err
    | Non_prenex_polymorphism : Ty.t -> Ast.t err
    | Inference_forbidden :
        Ty.Var.t * wildcard_source * Ty.t -> Ast.t err
    | Inference_conflict :
        Ty.Var.t * wildcard_source * Ty.t * Ty.t list -> Ast.t err
    | Inference_scope_escape :
        Ty.Var.t * wildcard_source * Ty.Var.t * reason option -> Ast.t err
    | Unbound_type_wildcards :
        (Ty.Var.t * wildcard_source list) list -> Ast.t err
    | Incoherent_type_redefinition :
        Id.t * Ty.Const.t * reason * int -> Stmt.def err
    | Incoherent_term_redefinition :
        Id.t * T.Const.t * reason * Ty.t -> Stmt.def err
    | Uncaught_exn : exn * Printexc.raw_backtrace -> Ast.t err
    | Unhandled_ast : Ast.t err


  (* State & Environment *)
  (* ************************************************************************ *)

  type wildcard_hook = {
    src : wildcard_source;
    shape : wildcard_shape;
    bound : reason E.t;
  }

  (* Global, mutable state. *)
  type state = {

    mutable csts : cst M.t;
    (* association between dolmen ids and types/terms constants. *)
    mutable ttype_locs : reason R.t;
    (* stores reasons for typing of type constructors *)
    mutable const_locs : reason S.t;
    (* stores reasons for typing of constants *)
    mutable cstrs_locs : reason U.t;
    (* stores reasons for typing adt constructors *)
    mutable dstrs_locs : reason S.t;
    (* stores reasons for typing adt destrcutros/projectors *)
    mutable field_locs : reason V.t;
    (* stores reasons for typing record fields *)

    mutable custom : Hmap.map;
    (* heterogeneous map for theories to store custom information,
       all the while being kept in sync with changes in the global state. *)

  }

  (* The local environments used for type-checking. *)
  type env = {

    (* current file *)
    file : Loc.file;

    (* global state *)
    st        : state;

    (* Regular bound variables *)
    vars : env bound_var M.t;
    type_locs : reason E.t;
    term_locs : reason F.t;

    (* inferred variables *)
    inferred_vars : ty_var M.t ref;
    inferred_ty_locs : reason E.t ref;

    (* wildcards *)
    wildcards     : wildcard_hook list E.t ref;

    (* The current builtin symbols *)
    builtins : builtin_symbols;

    (* warnings *)
    warnings : warning -> unit;

    (* Typechecker configuration *)
    order     : order;
    poly      : poly;
    quants    : bool;
    expect    : expect;
    var_infer : var_infer;
    sym_infer : sym_infer;
    free_wildcards : free_wildcards;
  }

  (* Builtin symbols, i.e symbols understood by some theories,
     but which do not have specific syntax, so end up as special
     cases of application. *)
  and builtin_symbols = env -> symbol -> [ builtin_res | not_found ]

  (* Existencial wrapper for wranings. *)
  and warning =
    | Warning : env * 'a fragment * 'a warn -> warning

  (* Exitencial wrapper around errors *)
  and error =
    | Error : env * 'a fragment * 'a err -> error

  (* Convenient alias *)
  type 'a typer = env -> Ast.t -> 'a

  (* Convenient aliases *)
  type var = env bound_var
  type bound = [ var | cst | builtin ]


  (* Exceptions *)
  (* ************************************************************************ *)

  (* Internal exceptions *)
  exception Found of Ast.t * res
  exception Wildcard_bad_scope of Ty.Var.t * wildcard_source * Ty.Var.t
  exception Wildcard_bad_base of Ty.Var.t * wildcard_source * Ty.t * Ty.t list
  exception Wildcard_forbidden of Ty.Var.t * wildcard_source * Ty.t

  (* Exception for typing errors *)
  exception Typing_error of error


  (* Accessors to the env *)
  (* ************************************************************************ *)

  let state env = env.st

  let var_infer env = env.var_infer

  let sym_infer env = env.sym_infer

  (* Builtin helpers *)
  (* ************************************************************************ *)

  let builtin_ttype ?(meta=()) apply : [> builtin_common] =
    `Ttype (meta, apply)

  let builtin_ty ?(meta=()) apply : [> builtin_common] =
    `Ty (meta, apply)

  let builtin_tags ?(meta=()) apply : [> builtin_common] =
    `Tags (meta, apply)

  let builtin_term ?(meta=`Total) apply : [> builtin_common] =
    `Term (meta, apply)

  (* Warnings/Error helpers *)
  (* ************************************************************************ *)

  let _warn env fragment w =
    env.warnings (Warning (env, fragment, w))

  let _error env fragment e =
    raise (Typing_error (Error (env, fragment, e)))

  let loc env loc : Loc.full =
    { file = env.file; loc; }

  let fragment_loc :
    type a. env -> a fragment -> Loc.full = fun env fg ->
    let loc =
      match fg with
      | Ast { loc; _ } -> loc
      | Def d -> d.loc
      | Defs { contents = []; _ } -> Loc.no_loc
      | Defs { contents = d :: _; _ } -> d.loc
      | Decl d -> decl_loc d
      | Decls { contents = []; _ } -> Loc.no_loc
      | Decls { contents = d :: _; _ } -> decl_loc d
      | Located l -> l
    in
    { file = env.file;
      loc = loc; }

  let rec find_pattern_ast pat asts parsed =
    match asts, parsed with
    | [], _ | _, [] -> assert false
    | (ast, _) :: r, (t, _) :: r' ->
      if pat == t then ast else find_pattern_ast pat r r'

  (* Binding lookups *)
  (* ************************************************************************ *)

  let find_reason env (v : bound) =
    try
      let r =
        match v with
        | `Builtin `Reserved (reason, _) -> Reserved reason
        | `Builtin _ -> Builtin
        | `Ty_var v -> E.find v env.type_locs
        | `Term_var v -> F.find v env.term_locs
        | `Letin (_, _, v, _) -> F.find v env.term_locs
        | `Ty_cst c -> R.find c env.st.ttype_locs
        | `Term_cst c -> S.find c env.st.const_locs
        | `Cstr c -> U.find c env.st.cstrs_locs
        | `Dstr c -> S.find c env.st.dstrs_locs
        | `Field f -> V.find f env.st.field_locs
      in
      Some r
    with Not_found -> None

  let with_reason reason bound : binding =
    match (bound : [ bound | not_found ]) with
    | `Not_found -> `Not_found
    | `Builtin `Infer (reason, _, _) -> `Reserved reason
    | `Builtin `Ttype _ -> `Builtin `Ttype
    | `Builtin `Ty _ -> `Builtin `Ty
    | `Builtin `Term _ -> `Builtin `Term
    | `Builtin `Tags _ -> `Builtin `Tag
    | `Builtin `Reserved (reason, `Solver) -> `Reserved reason
    | `Builtin `Reserved (reason, `Term_cst _) -> `Reserved reason
    | `Ty_var v -> `Variable (`Ty (v, reason))
    | `Term_var v -> `Variable (`Term (v, reason))
    | `Letin (_, _, v, _) -> `Variable (`Term (v, reason))
    | `Ty_cst c -> `Constant (`Ty (c, reason))
    | `Term_cst c -> `Constant (`Term (c, reason))
    | `Cstr c -> `Constant (`Cstr (c, reason))
    | `Dstr c -> `Constant (`Dstr (c, reason))
    | `Field f -> `Constant (`Field (f, reason))

  let binding_reason binding : reason option =
    match (binding : binding) with
    | `Not_found -> assert false
    | `Builtin _ -> Some Builtin
    | `Reserved reason -> Some (Reserved reason)
    | `Variable `Ty (_, reason)
    | `Variable `Term (_, reason)
    | `Constant `Ty (_, reason)
    | `Constant `Term (_, reason)
    | `Constant `Cstr (_, reason)
    | `Constant `Dstr (_, reason)
    | `Constant `Field (_, reason)
      -> reason

  let _shadow env fragment id
      (old : bound) reason (bound : [< bound]) =
    let old_binding =
      with_reason (find_reason env old) (old :> [bound | not_found])
    in
    let new_binding = with_reason (Some reason) (bound :> [bound | not_found]) in
    _warn env fragment (Shadowing (id, old_binding, new_binding))

  let find_var env name : [var | not_found] =
    match M.find_opt name env.vars with
    | Some (#var as res) -> res
    | None ->
      begin match M.find_opt name !(env.inferred_vars) with
        | Some (#ty_var as res) -> res
        | None -> `Not_found
      end

  let find_global_st st id : [ cst | not_found ] =
    match M.find_opt id st.csts with
    | Some res -> (res :> [cst | not_found])
    | None -> `Not_found

  let find_global env id : [cst | not_found] =
    find_global_st env.st id

  let find_builtin env id : [builtin | not_found] =
    match env.builtins env (Id id) with
    | `Not_found -> `Not_found
    | #builtin_res as res -> `Builtin res

  let find_bound env id : [ bound | not_found ] =
    match find_var env id with
    | #var as res -> (res :> [ bound | not_found ])
    | `Not_found ->
      begin match find_global env id with
        | #cst as res -> (res :> [ bound | not_found ])
        | `Not_found ->
          (find_builtin env id :> [ bound | not_found ])
      end


  (* Convenience functions *)
  (* ************************************************************************ *)

  let _expected env s t res =
    _error env (Ast t) (Expected (s, res))

  let _bad_op_arity env s n m t =
    _error env (Ast t) (Bad_op_arity (s, [n], m))

  let _bad_ty_arity env f n t =
    _error env (Ast t) (Bad_ty_arity (f, n))

  let _bad_term_arity env f expected actual t =
    _error env (Ast t) (Bad_term_arity (f, expected, actual))

  let _bad_cstr_arity env c expected actual t =
    _error env (Ast t) (Bad_cstr_arity (c, expected, actual))

  let _bad_poly_arity env ast ty_vars tys =
    _error env (Ast ast) (Bad_poly_arity (ty_vars, tys))

  let _redundant_pattern env ast pat =
    _warn env (Ast ast) (Redundant_pattern pat)

  let _partial_pattern_match env ast missing =
    _error env (Ast ast) (Partial_pattern_match missing)

  let _over_application env ast over_args =
    _error env (Ast ast) (Over_application over_args)

  let _ty_var_app env v t =
    _error env (Ast t) (Ty_var_application v)

  let _var_app env v t =
    _error env (Ast t) (Var_application v)

  let _type_mismatch env t ty ast =
    _error env (Ast ast) (Type_mismatch (t, ty))

  let _wrong_sum_type env ast cstr ty =
    _error env (Ast ast) (Mismatch_sum_type (cstr, ty))

  let _record_type_mismatch env f ty_c ast =
    _error env (Ast ast) (Mismatch_record_type (f, ty_c))

  let _field_repeated env f ast =
    _error env (Ast ast) (Repeated_record_field f)

  let _field_missing env f ast =
    _error env (Ast ast) (Missing_record_field f)

  let _cannot_infer_var_in_binding_pos env t =
    _error env (Ast t) (Var_in_binding_pos_underspecified)

  let _unknown_builtin env ast b =
    _error env (Ast ast) (Unhandled_builtin b)

  let _uncaught_exn env ast exn bt =
    _error env (Ast ast) (Uncaught_exn (exn, bt))

  let _cannot_find ?(hint="") env ast s =
    _error env (Ast ast) (Cannot_find (s, hint))

  let _id_def_conflict env loc id binding =
    _error env (Located loc) (Id_definition_conflict (id, binding))

  let _non_prenex_polymorphism env ast ty =
    _error env (Ast ast) (Non_prenex_polymorphism ty)

  let _scope_escape_in_wildcard env ast w w_src v =
    let r = find_reason env (`Ty_var v) in
    _error env (Ast ast) (Inference_scope_escape (w, w_src, v, r))

  let _inference_conflict env ast w w_src inferred allowed =
    _error env (Ast ast) (Inference_conflict (w, w_src, inferred, allowed))

  let _inference_forbidden env ast w w_src inferred =
    _error env (Ast ast) (Inference_forbidden (w, w_src, inferred))

  let _incoherent_type_redefinition env def cst reason n =
    _error env (Def def)
      (Incoherent_type_redefinition (def.id, cst, reason, n))

  let _incoherent_term_redefinition env def cst reason ty =
    _error env (Def def)
      (Incoherent_term_redefinition (def.id, cst, reason, ty))

  let _wrap_exn env ast = function
    | Ty.Prenex_polymorphism ty ->
      _non_prenex_polymorphism env ast ty
    | T.Wrong_type (t, ty) ->
      _type_mismatch env t ty ast
    | T.Wrong_sum_type (cstr, ty) ->
      _wrong_sum_type env ast cstr ty
    | T.Wrong_record_type (f, c) ->
      _record_type_mismatch env f c ast
    | T.Field_repeated f ->
      _field_repeated env f ast
    | T.Field_missing f ->
      _field_missing env f ast
    | T.Over_application over_args ->
      _over_application env ast over_args
    | T.Bad_poly_arity (vars, args) ->
      _bad_poly_arity env ast vars args
    | T.Partial_pattern_match missing ->
      _partial_pattern_match env ast missing
    | Wildcard_bad_scope (w, w_src, v) ->
      _scope_escape_in_wildcard env ast w w_src v
    | Wildcard_bad_base (w, w_src, inferred, allowed) ->
      _inference_conflict env ast w w_src inferred allowed
    | Wildcard_forbidden (w, w_src, inferred) ->
      _inference_forbidden env ast w w_src inferred
    | (Typing_error _) as exn ->
      raise exn
    | exn ->
      let bt = Printexc.get_raw_backtrace () in
      _uncaught_exn env ast exn bt

  let[@inline] _wrap env ast f arg =
    try f arg with exn -> _wrap_exn env ast exn

  let[@inline] _wrap2 env ast f a b =
    let[@inline] aux () = f a b in
    (_wrap[@inlined]) env ast aux ()

  let[@inline] _wrap3 env ast f a b c =
    let[@inline] aux () = f a b c in
    (_wrap[@inlined]) env ast aux ()


  (* Global Environment *)
  (* ************************************************************************ *)

  let new_state () = {
    csts = M.empty;
    ttype_locs = R.empty;
    const_locs = S.empty;
    cstrs_locs = U.empty;
    dstrs_locs = S.empty;
    field_locs = V.empty;
    custom = Hmap.empty;
  }

  let copy_state st = {
    csts = st.csts;
    custom = st.custom;
    ttype_locs = st.ttype_locs;
    const_locs = st.const_locs;
    cstrs_locs = st.cstrs_locs;
    dstrs_locs = st.dstrs_locs;
    field_locs = st.field_locs;
  }

  (* Var/Const creation *)
  let var_name _env name =
    match (name : Dolmen.Std.Name.t) with
    | Simple name -> name
    (* TODO: proper errors *)
    | Indexed _ -> assert false
    | Qualified _ -> assert false

  let cst_path _env name =
    match (name : Dolmen.Std.Name.t) with
    | Indexed _ -> assert false
    | Simple name ->
      Dolmen.Std.Path.global name
    | Qualified { path; basename; } ->
      Dolmen.Std.Path.absolute path basename

  let mk_ty_var env name =
    Ty.Var.mk (var_name env name)

  let mk_term_var env name ty =
    T.Var.mk (var_name env name) ty

  let mk_ty_cst env name arity =
    Ty.Const.mk (cst_path env name) arity

  let mk_term_cst env name ty =
    T.Const.mk (cst_path env name) ty


  (* Const declarations *)
  let add_global env fragment id reason (v : cst) =
    begin match find_bound env id with
      | `Not_found -> ()
      | `Builtin `Infer _ -> () (* inferred builtins are meant to be shadowed/replaced *)
      | #bound as old -> _shadow env fragment id old reason v
    end;
    env.st.csts <- M.add id v env.st.csts

  let decl_ty_const env fg id c reason =
    add_global env fg id reason (`Ty_cst c);
    env.st.ttype_locs <- R.add c reason env.st.ttype_locs

  let decl_term_const env fg id c reason =
    add_global env fg id reason (`Term_cst c);
    env.st.const_locs <- S.add c reason env.st.const_locs

  let decl_term_cstr env fg id c reason =
    add_global env fg id reason (`Cstr c);
    env.st.cstrs_locs <- U.add c reason env.st.cstrs_locs

  let decl_term_dstr env fg id d reason =
    add_global env fg id reason (`Dstr d);
    env.st.dstrs_locs <- S.add d reason env.st.dstrs_locs

  let decl_term_field env fg id f reason =
    add_global env fg id reason (`Field f);
    env.st.field_locs <- V.add f reason env.st.field_locs


  (* Custom theory data in the global state *)
  let get_global_custom_state st key = Hmap.get st.custom key
  let get_global_custom env key = get_global_custom_state env.st key

  let set_global_custom_state st key value =
    st.custom <- Hmap.set st.custom key value
  let set_global_custom env key value =
    set_global_custom_state env.st key value


  (* Local Environment *)
  (* ************************************************************************ *)

  let global = new_state ()

  (* Make a new empty environment *)
  let empty_env
      ?(st=global)
      ?(expect=Anything)
      ?(var_infer={
          var_hook = ignore;
          infer_unbound_vars = No_inference;
          infer_type_vars_in_binding_pos = true;
          infer_term_vars_in_binding_pos = Wildcard Any_in_scope;
        })
      ?(sym_infer={
          sym_hook = ignore;
          infer_type_csts = true;
          infer_term_csts = Wildcard Any_in_scope;
        })
      ?(order=Higher_order)
      ?(poly=Flexible)
      ?(quants=true)
      ?(free_wildcards=Forbidden)
      ~warnings ~file
      builtins =
    let inferred_vars = ref M.empty in
    let inferred_ty_locs = ref E.empty in
    let wildcards = ref E.empty in
    {
      file; st; builtins; warnings;

      vars = M.empty;
      type_locs = E.empty;
      term_locs = F.empty;

      inferred_vars; inferred_ty_locs; wildcards;

      order; poly; quants;
      var_infer; sym_infer;
      expect; free_wildcards;
    }

  let split_env_for_def env =
    let inferred_vars = ref M.empty in
    let inferred_ty_locs = ref E.empty in
    let wildcards = ref E.empty in
    { env with inferred_vars; inferred_ty_locs; wildcards; }

  (* add a global inferred var *)
  let add_inferred_type_var env id v ast =
    let reason = Inferred (env.file, ast) in
    begin match find_bound env id with
      | `Not_found -> ()
      | #bound as old ->
        _shadow env (Ast ast) id old reason (`Ty_var v)
    end;
    env.inferred_vars := M.add id (`Ty_var v) !(env.inferred_vars);
    env.inferred_ty_locs := E.add v reason !(env.inferred_ty_locs);
    ()

  (* Add local variables to environment *)
  let add_type_var env id v ast =
    let reason = Bound (env.file, ast) in
    begin match find_bound env id with
      | `Not_found -> ()
      | #bound as old ->
        _shadow env (Ast ast) id old reason (`Ty_var v)
    end;
    { env with
      vars = M.add id (`Ty_var v) env.vars;
      type_locs = E.add v reason env.type_locs;
    }

  let add_type_vars env l =
    let env =
      List.fold_left (fun acc (id, v, ast) ->
          add_type_var acc id v ast
        ) env l
    in
    let l' = List.map (fun (_, v, _) -> v) l in
    l', env

  let add_term_var env id v ast =
    let reason = Bound (env.file, ast) in
    begin match find_bound env id with
      | `Not_found -> ()
      | #bound as old ->
        _shadow env (Ast ast) id old reason (`Term_var v)
    end;
    { env with
      vars = M.add id (`Term_var v) env.vars;
      term_locs = F.add v reason env.term_locs;
    }

  let bind_term_var env id e v t ast =
    let reason = Bound (env.file, ast) in
    begin match find_bound env id with
      | `Not_found -> ()
      | #bound as old ->
        _shadow env (Ast ast) id old reason (`Term_var v)
    end;
    let t' = T.bind v t in
    { env with
      vars = M.add id (`Letin (env, e, v, t')) env.vars;
      term_locs = F.add v reason env.term_locs;
    }


  (* Typo suggestion *)
  (* ************************************************************************ *)

  let suggest ~limit env id =
    let name id = Format.asprintf "%a" Id.print id in
    let automaton = Spelll.of_string ~limit (name id) in
    let aux id _ acc =
      if Spelll.match_with automaton (name id)
      then id :: acc
      else acc
    in
    M.fold aux env.st.csts (M.fold aux env.vars [])



  (* Type inference and wildcards *)
  (* ************************************************************************ *)

  let get_allowed_shapes w_map_ref v =
    match E.find v !w_map_ref with
    | shapes -> shapes
    | exception Not_found -> []

  let is_shape_redundant shapes ({ shape; src = _; bound; } as mark) =
    let subsumes_new ({ shape = shape'; src = _; bound = bound'; } as mark') =
      mark == mark' ||
      (shape == shape' && bound == bound') ||
      match shape, shape' with
      | Forbidden, Forbidden -> true
      | Any_in_scope, Any_in_scope -> bound == bound'
      | Any_base { allowed = l; preferred = p; },
        Any_base { allowed = l'; preferred = p'; } ->
        Ty.equal p p' &&
        List.for_all (fun ty -> List.exists (Ty.equal ty) l') l
      | _ -> false
    in
    List.exists subsumes_new shapes

  let rec wildcard_hook w_map_ref w ty =
    let shapes = get_allowed_shapes w_map_ref w in
    w_map_ref := E.remove w !w_map_ref;
    check_shapes w_map_ref w ty shapes

  and check_shapes w_map_ref w ty shapes =
    let fv = lazy (Ty.fv ty) in
    List.iter (check_shape w_map_ref fv w ty) shapes

  and check_shape w_map_ref fv w ty = function
    | { shape = Forbidden; src; bound = _; } ->
      raise (Wildcard_forbidden (w, src, ty))
    | { shape = Any_in_scope; src; bound = bound_vars; } as mark ->
      List.iter (fun v ->
          if Ty.Var.is_wildcard v then begin
            transfer_hook w_map_ref mark v
          end else if not (E.mem v bound_vars) then
            raise (Wildcard_bad_scope (w, src, v))
        ) (Lazy.force fv)
    | { shape = Any_base { allowed; preferred = _; }; src; bound = _; } as mark ->
      begin match Ty.view ty with
        | `Wildcard v -> transfer_hook w_map_ref mark v
        | _ ->
          if List.exists (Ty.equal ty) allowed then ()
          else raise (Wildcard_bad_base (w, src, ty, allowed))
      end
    | { shape = Arrow { arg_shape; ret_shape; }; src; bound = bound_vars; } as mark ->
      begin match Ty.view ty with
        | `Wildcard v -> transfer_hook w_map_ref mark v
        | `Arrow (ty_args, ty_ret) ->
          List.iter (transfer_shape w_map_ref arg_shape (Arg_of src) bound_vars) ty_args;
          transfer_shape w_map_ref ret_shape (Ret_of src) bound_vars ty_ret
        | _ -> transfer_shape w_map_ref ret_shape src bound_vars ty
      end

  and transfer_shape w_map_ref shape src bound ty =
    let mark = { shape; src; bound; } in
    match Ty.view ty with
    | `Wildcard w -> transfer_hook w_map_ref mark w
    | _ ->
      let w = Ty.Var.wildcard () in
      check_shapes w_map_ref w ty [mark]

  and transfer_hook w_map_ref mark v =
    let l = get_allowed_shapes w_map_ref v in
    if l = [] then Ty.add_wildcard_hook ~hook:(wildcard_hook w_map_ref) v;
    if not (is_shape_redundant l mark) then
      w_map_ref := E.add v (mark :: l) !w_map_ref

  (* create a wildcard *)
  let wildcard_var env src shape =
    let w = Ty.Var.wildcard () in
    let mark = { shape; src; bound = env.type_locs; } in
    env.wildcards := E.add w [mark] !(env.wildcards);
    Ty.add_wildcard_hook ~hook:(wildcard_hook env.wildcards) w;
    w

  let wildcard env src shape =
    let w = wildcard_var env src shape in
    Ty.of_var w

  (* Try and set a wildcard according to one of its shapes *)
  let rec try_set_wildcard_shape w = function
    | [] -> false
    | Any_base { preferred; allowed = _; } :: _ ->
      Ty.set_wildcard w preferred; true
    | Arrow { ret_shape; arg_shape = _; } :: r ->
      try_set_wildcard_shape w (ret_shape :: r)
    | _ :: r ->
      try_set_wildcard_shape w r

  (* "pop" a wildcard out of the set of watched wildcards *)
  let pop_wildcard env =
    let w, l = E.choose !(env.wildcards) in
    env.wildcards := E.remove w !(env.wildcards);
    w, l

  (* ensure all wildcards are set *)
  let rec set_wildcards_and_return_free_wildcards state acc =
    match pop_wildcard state with
    | exception Not_found -> acc
    | w, l ->
      let shapes = List.map (fun { shape; src = _; bound = _; } -> shape) l in
      let acc =
        if try_set_wildcard_shape w shapes then acc
        else begin
          let sources =
            List.map (fun { src; shape = _; bound = _; } -> src) l
          in
          ((w, sources) :: acc)
        end
      in
      set_wildcards_and_return_free_wildcards state acc


  (* Typing explanation *)
  (* ************************************************************************ *)

  let find_ty_var_reason env v =
    try E.find v env.type_locs
    with Not_found -> E.find v !(env.inferred_ty_locs)

  let _unused_type env kind v =
    if Ty.Var.is_wildcard v then ()
    (* whether a wildcard is used or not is complex, and not really useful.
       instead, we produce errors when wildcards are not used correctly, or
       escape their scope, which is more informative. *)
    else match find_ty_var_reason env v with
    (* Variable bound or inferred *)
    | Bound (_, t) | Inferred (_, t) ->
      _warn env (Ast t) (Unused_type_variable (kind, v))
    (* variables should not be declare-able nor builtin *)
    | Builtin | Reserved _ | Declared _ | Defined _
    | Implicit_in_def _ | Implicit_in_decl _ | Implicit_in_term _ ->
      assert false

  let find_term_var_reason env v =
    F.find v env.term_locs

  let _unused_term env kind v =
    match find_term_var_reason env v with
    (* Variable bound or inferred *)
    | Bound (_, t) | Inferred (_, t) ->
      _warn env (Ast t) (Unused_term_variable (kind, v))
    (* variables should not be declare-able nor builtin,
       and we do not use any term wildcards. *)
    | Builtin | Reserved _ | Declared _ | Defined _
    | Implicit_in_def _ | Implicit_in_decl _ | Implicit_in_term _ ->
      assert false


  (* Wrappers for expression building *)
  (* ************************************************************************ *)

  (* unwrap results *)
  let unwrap_ty env ast = function
    | Ty ty -> ty
    | res -> _expected env "type" ast (Some res)

  let unwrap_term env ast = function
    | Term t -> t
    | res -> _expected env "term" ast (Some res)

  (* Type exploration *)
  let arity ty =
    let n_ty, ty =
      match Ty.view ty with
      | `Pi (vars, ty) -> List.length vars, ty
      | _ -> 0, ty
    in
    let n_term, _ty =
      match Ty.view ty with
      | `Arrow (params, ty) -> List.length params, ty
      | _ -> 0, ty
    in
    n_ty, n_term

  (* Un-polymorphize a term, by applying it to the adequate
     number of type wildcards *)
  let monomorphize env ast t =
    match Ty.view (T.ty t) with
    | `Pi (vars, _) ->
      let n_ty = List.length vars in
      if n_ty = 0 then t
      else begin
        let src = Added_type_argument ast in
        let ty_l =
          Misc.Lists.init n_ty
            (fun _ -> wildcard env src Any_in_scope)
        in
        _wrap3 env ast T.apply t ty_l []
      end
    | _ -> t

  let check_not_poly env ast t =
    match Ty.view (T.ty t) with
    | `Pi _ -> _error env (Ast ast) Polymorphic_function_argument
    | _ -> t

  (* Split arguments for first order application *)
  let split_fo_args env ast n_ty n_t args =
    let n_args = List.length args in
    match env.poly with
    | Explicit ->
      if n_args = n_ty + n_t then
        `Ok (Misc.Lists.take_drop n_ty args)
      else
        `Bad_arity ([n_ty + n_t], n_args)
    | Implicit ->
      if n_args = n_t then begin
        let src = Added_type_argument ast in
        let tys =
          Misc.Lists.init n_ty
            (fun _ -> wildcard env src Any_in_scope)
        in
        `Fixed (tys, args)
      end else
        `Bad_arity ([n_t], n_args)
    | Flexible ->
      if n_args = n_ty + n_t then
        `Ok (Misc.Lists.take_drop n_ty args)
      else if n_args = n_t then begin
        let src = Added_type_argument ast in
        let tys =
          Misc.Lists.init n_ty
            (fun _ -> wildcard env src Any_in_scope)
        in
        `Fixed (tys, args)
      end else
        `Bad_arity ([n_t; n_ty + n_t], n_args)

  (* Split arguments for higher order application *)
  let split_ho_args env ast n_ty args =
    let explicit args =
      let rec aux tys acc = function
        | [] -> List.rev tys, List.rev acc
        | (ast, Term t) :: r -> aux tys ((ast, t) :: acc) r
        | (ast, ((Ty _) as res)) :: _ ->
          _expected env "a term" ast (Some res)
        | (ast, ((Ttype | Tags _) as res)) :: _ ->
          _expected env "a type or a term" ast (Some res)
      and aux_ty acc = function
        | (_, Ty ty) :: r -> aux_ty (ty :: acc) r
        | l -> aux acc [] l
      in
      aux_ty [] args
    in
    let implicit ast n_ty args =
      let src = Added_type_argument ast in
      let ty_l =
        Misc.Lists.init n_ty
          (fun _ -> wildcard env src Any_in_scope)
      in
      let t_l = List.map (function
          | ast, Term t -> ast, t
          | ast, res -> _expected env "a term" ast (Some res)
        ) args
      in
      ty_l, t_l
    in
    let ty_l, t_l =
      match env.poly with
      | Explicit -> explicit args
      | Implicit -> implicit ast n_ty args
      | Flexible ->
        begin match args with
          | (_, Ty _) :: _ -> explicit args
          | _ -> implicit ast n_ty args
        end
    in
    let t_l =
      match env.poly with
      | Explicit ->
        List.map (fun (ast, t) -> check_not_poly env ast t) t_l
      | Implicit | Flexible ->
        List.map (fun (ast, t) -> monomorphize env ast t) t_l
    in
    ty_l, t_l


  (* Wrapper around record creation *)
  let create_record env ast l =
    _wrap env ast T.record l

  let create_record_with env ast t l =
    _wrap2 env ast T.record_with t l

  let create_record_access env ast t field =
    _wrap2 env ast T.apply_field field t

  let used_var_tag = Tag.create ()

  let mark_ty_var_as_used v =
    Ty.Var.set_tag v used_var_tag ()

  let mark_term_var_as_used v =
    T.Var.set_tag v used_var_tag ()

  (* Emit warnings for quantified variables that are unused *)
  let check_used_ty_var ~kind env v =
    match Ty.Var.get_tag v used_var_tag with
    | Some () -> Ty.Var.unset_tag v used_var_tag
    | None -> _unused_type env kind v

  let check_used_term_var ~kind env v =
    match T.Var.get_tag v used_var_tag with
    | Some () -> T.Var.unset_tag v used_var_tag
    | None -> _unused_term env kind v

  (* Wrappers for creating binders *)
  let mk_let env ast mk l body =
    List.iter (fun (v, _) ->
        check_used_term_var ~kind:`Let_bound env v
      ) l;
    _wrap2 env ast mk l body

  let mk_binder env b ast mk (ty_vars, t_vars) body =
    begin match (b : Ast.binder) with
    | Ex | All ->
      if not env.quants then
        _error env (Ast ast) Forbidden_quantifier
    | Fun ->
      begin match env.order with
        | Higher_order -> ()
        | First_order -> _error env (Ast ast) Higher_order_type
      end
    | Pi | Arrow | Let_seq | Let_par | Choice | Description -> ()
    end;
    List.iter (check_used_ty_var ~kind:`Quantified env) ty_vars;
    List.iter (check_used_term_var ~kind:`Quantified env) t_vars;
    _wrap2 env ast mk (ty_vars, t_vars) body


  let free_wildcards_to_quant_vars =
    let wildcard_univ_counter = ref 0 in
    (fun ctx env wildcards ->
       Misc.Lists.fold_left_map (fun env (w, _) ->
           incr wildcard_univ_counter;
           let v =
             Ty.Var.mk (Format.asprintf "w%d" !wildcard_univ_counter)
           in
           let env =
             match ctx with
             | None -> env
             | Some ctx ->
               let reason =
                 match ctx with
                 | `Def def -> Implicit_in_def (env.file, def)
                 | `Decl decl -> Implicit_in_decl (env.file, decl)
                 | `Term ast -> Implicit_in_term (env.file, ast)
               in
               { env with type_locs = E.add v reason env.type_locs; }
           in
           Ty.set_wildcard w (Ty.of_var v);
           (* the wildcard was generated from the term being typechecked,
              so it is at least used where it was generated. *)
           mark_ty_var_as_used v;
           env, v
         ) env wildcards
    )

  let finalize_wildcards ctx env ast =
    let free_wildcards =
      _wrap2 env ast set_wildcards_and_return_free_wildcards env []
    in
    begin match free_wildcards, env.free_wildcards with
      | [], _ -> `No_free_wildcards
      | tys, Implicitly_universally_quantified ->
        (* Ensure that the wildcards are properly instantiated, so that
           they cannot be instantiated after we have built the
           quantification *)
        let env, vars = free_wildcards_to_quant_vars ctx env tys in
        `Univ (env, tys, vars)
      | free_wildcards, Forbidden ->
        _error env (Ast ast) (Unbound_type_wildcards free_wildcards)
    end

  let finalize_wildcards_prop ctx env ast prop =
    match finalize_wildcards (Some ctx) env ast with
    | `No_free_wildcards -> env, prop
    | `Univ (env, _, vars) ->
      let res = _wrap2 env ast T.all (vars, []) prop in
      env, res

  let finalize_wildcards_ty ctx env ast ty =
    match finalize_wildcards (Some ctx) env ast with
    | `No_free_wildcards -> env, ty
    | `Univ (env, _, vars) -> env, Ty.pi vars ty

  let finalize_wildcards_def ctx env ast =
    match finalize_wildcards (Some ctx) env ast with
    | `No_free_wildcards -> env, []
    | `Univ (env, _, vars) -> env, vars

  let check_no_free_wildcards env ast =
    match finalize_wildcards None env ast with
    | `No_free_wildcards -> ()
    | `Univ (env, free_wildcards, _) ->
      _error env (Ast ast) (Unbound_type_wildcards free_wildcards)


  (* Tag application *)
  (* ************************************************************************ *)

  let set_tag env ast tag v res =
    match (res : res) with
    | Ttype -> _error env (Ast ast) Cannot_tag_ttype
    | Tags _ -> _error env (Ast ast) Cannot_tag_tag
    | Ty ty -> Ty.set_tag ty tag v
    | Term t -> T.set_tag t tag v

  let add_tag env ast tag v res =
    match (res : res) with
    | Ttype -> _error env (Ast ast) Cannot_tag_ttype
    | Tags _ -> _error env (Ast ast) Cannot_tag_tag
    | Ty ty -> Ty.add_tag ty tag v
    | Term t -> T.add_tag t tag v

  (* Small helpers *)
  (* ************************************************************************ *)

  let expect_anything env =
    match env.expect with
    | Anything -> env
    | _ -> { env with expect = Anything; }

  let expect_type env =
    match env.expect with
    | Type -> env
    | _ -> { env with expect = Type; }

  let expect_term env =
    match env.expect with
    | Term -> env
    | _ -> { env with expect = Term; }

  let[@inline] wrap_attr apply_attr env ast f =
    match ast.Ast.attr with
    | [] -> f ast
    | l -> apply_attr env (f ast) ast l

  let with_var_infer env var_infer = { env with var_infer }
  let with_sym_infer env sym_infer = { env with sym_infer }

  (* Expression parsing *)
  (* ************************************************************************ *)

  let rec parse_expr (env : env) t : res =
    let[@inline] aux t = parse_expr_aux env t in
    (wrap_attr[@inlined]) apply_attr env t aux

  and parse_expr_aux env = function
    (* Ttype *)
    | { Ast.term = Ast.Builtin Ast.Ttype; _ } ->
      Ttype

    (* Wildcards should only occur in place of types *)
    | { Ast.term = Ast.Builtin Ast.Wildcard; _ } as ast ->
      Ty (wildcard env (From_source ast) Any_in_scope)

    (* Arrows *)
    | { Ast.term = Ast.Binder (Ast.Arrow, args, ret); _ } as ast ->
      parse_arrow env ast [args] ret

    (* Binders *)
    | { Ast.term = Ast.Binder (Ast.Fun, _, _); _ } as ast ->
      parse_binder parse_term T.lam Ast.Fun env ast [] [] ast

    | { Ast.term = Ast.Binder (Ast.All, _, _); _ } as ast ->
      parse_binder parse_prop T.all Ast.All env ast [] [] ast

    | { Ast.term = Ast.Binder (Ast.Ex, _, _); _ } as ast ->
      parse_binder parse_prop T.ex Ast.Ex env ast [] [] ast

    | ({ Ast.term = Ast.Binder (Ast.Let_seq, vars, f); _ } as ast)
    | ({ Ast.term = Ast.Binder (Ast.Let_par, ([_] as vars), f); _ } as ast) ->
      parse_let_seq env ast [] f vars

    | { Ast.term = Ast.Binder (Ast.Let_par, vars, f); _ } as ast ->
      parse_let_par env ast [] f vars

    (* Pattern matching *)
    | { Ast.term = Ast.Match (scrutinee, branches); _ } as ast ->
      parse_match env ast scrutinee branches

    (* ADT operations *)
    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Adt_check; _ }, l); _ } as ast ->
      parse_adt_check env ast l

    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Adt_project; _ }, l); _ } as ast ->
      parse_adt_project env ast l

    (* Record creation *)
    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Record; _ }, l); _ } as ast ->
      parse_record env ast l

    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Record_with; _ }, l); _ } as ast ->
      parse_record_with env ast l

    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Record_access; _ }, l); _ } as ast ->
      parse_record_access env ast l

    (* Type annotations *)
    | { Ast.term = Ast.Colon (a, expected); _ } ->
      parse_ensure env a expected

    (* Sometimes parser creates extra applications *)
    | { Ast.term = Ast.App (t, []); _ } ->
      parse_expr env t

    (* Application *)
    | { Ast.term = Ast.App (f, args); _ } as ast ->
      parse_app env ast f args

    (* Symbols *)
    | { Ast.term = Ast.Symbol s; _ } as ast ->
      parse_symbol env ast s ast

    (* Builtin *)
    | { Ast.term = Ast.Builtin b; _ } as ast ->
      parse_builtin env ast b

    (* Other cases *)
    | ast -> _error env (Ast ast) Unhandled_ast

  and apply_attr env res ast l =
    let () = List.iter (function
        | Set (tag, v) -> set_tag env ast tag v res
        | Add (tag, v) -> add_tag env ast tag v res
      ) (parse_attrs env [] l) in
    res

  and parse_attr env ast =
    match parse_expr (expect_anything env) ast with
    | Tags l -> l
    | res -> _expected env "tag" ast (Some res)

  and parse_attrs env acc = function
    | [] -> acc
    | a :: r ->
      parse_attrs env (parse_attr env a @ acc) r

  and parse_var_in_binding_pos env = function
    | { Ast.term = Ast.Symbol s; _ } as t ->
      infer_var_in_binding_pos env t s
    | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s; _ }, e); _ } ->
      begin match parse_expr env e with
        | Ttype -> `Ty (s, mk_ty_var env (Id.name s))
        | Ty ty -> `Term (s, mk_term_var env (Id.name s) ty)
        | res -> _expected env "type (or Ttype)" e (Some res)
      end
    | t -> _expected env "(typed) variable" t None

  and parse_arrow env ast acc ret =
    match env.order with
    | First_order -> _error env (Ast ast) Higher_order_type
    | Higher_order ->
      let[@inline] aux t = parse_arrow_aux env ast acc t in
      (wrap_attr[@inlined]) apply_attr env ret aux

  and parse_arrow_aux env ast acc = function
    | { Ast.term = Ast.Binder (Ast.Arrow, args, ret'); _ } ->
      parse_arrow env ast (args :: acc) ret'
    | ret ->
      let args = List.flatten (List.rev acc) in
      let args = List.map (parse_ty env) args in
      let ret = parse_ty env ret in
      Ty (_wrap2 env ast Ty.arrow args ret)

  and parse_binder_vars env l =
    let ttype_vars, typed_vars, env' = List.fold_left (
        fun (l1, l2, acc) v ->
          match parse_var_in_binding_pos acc v with
          | `Ty (id, v') ->
            let acc' = add_type_var acc id v' v in
            (v' :: l1, l2, acc')
          | `Term (id, v') ->
            let acc' = add_term_var acc id v' v in
            (l1, v' :: l2, acc')
      ) ([], [], env) l in
    List.rev ttype_vars, List.rev typed_vars, env'

  and parse_binder parse_inner mk b env ast ttype_acc ty_acc body_ast =
    let [@inline] aux t =
      parse_binder_aux parse_inner mk b env ast ttype_acc ty_acc t
    in
    (wrap_attr[@inlined]) apply_attr env body_ast aux

  and parse_binder_aux parse_inner mk b env ast ttype_acc ty_acc = function
    | { Ast.term = Ast.Binder (b', vars, f); _ } when b = b' ->
      let ttype_vars, ty_vars, env' = parse_binder_vars env vars in
      parse_binder parse_inner mk b env' ast (ttype_acc @ ttype_vars) (ty_acc @ ty_vars) f
    | body_ast ->
      let body = parse_inner env body_ast in
      let f = mk_binder env b ast mk (ttype_acc, ty_acc) body in
      Term f

  and parse_match env ast scrutinee branches =
    let t = parse_term env scrutinee in
    let l = List.map (parse_branch (T.ty t) env) branches in
    (* small hack to get back the correct pattern ast for the warning *)
    let redundant t =
      let ast = find_pattern_ast t branches l in
      _redundant_pattern env ast t
    in
    Term (_wrap2 env ast (T.pattern_match ~redundant) t l)

  and parse_branch ty env (pattern, body) =
    let p, env = parse_pattern ty env pattern in
    let b = parse_term env body in
    (p, b)

  and parse_pattern ty env t =
    match t with
    | { Ast.term = Ast.Symbol s; _ } as ast_s ->
      parse_pattern_app ty env t ast_s s []
    | { Ast.term = Ast.App (
        ({ Ast.term = Ast.Symbol s; _ } as ast_s), args); _ } ->
      parse_pattern_app ty env t ast_s s args
    | _ -> _expected env "pattern" t None

  and parse_pattern_app ty env ast ast_s s args =
    match find_bound env s with
    | `Cstr c -> parse_pattern_app_cstr ty env ast c args
    | _ ->
      begin match args with
        | [] -> parse_pattern_var ty env ast_s s
        | _ -> _expected env "a variable (or an ADT constructor)" ast_s None
      end

  and parse_pattern_var ty env ast s =
    let v = mk_term_var env (Id.name s) ty in
    let env = add_term_var env s v ast in
    T.of_var v, env

  and parse_pattern_app_cstr ty env t c args =
    (* Inlined version of parse_app_cstr *)
    let n_ty, n_t = arity (T.Cstr.ty c) in
    let ty_args, t_l =
      match split_fo_args env t n_ty n_t args with
      | `Ok (l, l') ->
        (* We can't allow binding new type variables here *)
        let ty_args = List.map (parse_ty env) l in
        ty_args, l'
      | `Fixed (l, l') -> l, l'
      | `Bad_arity (expected, actual) ->
        _bad_cstr_arity env c expected actual t
    in
    (* Compute the expected types of arguments *)
    let ty_arity = _wrap3 env t T.Cstr.pattern_arity c ty ty_args in
    (* Pattern args are allowed to introduce new variables *)
    let t_args, env = parse_pattern_app_cstr_args env t_l ty_arity in
    let res = _wrap3 env t T.apply_cstr c ty_args t_args in
    res, env

  and parse_pattern_app_cstr_args env args args_ty =
    let l, env =
      List.fold_left2 (fun (l, env) arg ty ->
        let arg, env = parse_pattern ty env arg in
        (arg :: l, env)
        ) ([], env) args args_ty
    in
    List.rev l, env

  and parse_let_seq_end env ast acc = function
    | ({ Ast.term = Ast.Binder (Ast.Let_seq, vars, f'); _ } as f)
    | ({ Ast.term = Ast.Binder (Ast.Let_par, ([_] as vars), f'); _ } as f)->
      parse_let_seq env f acc f' vars
    | f ->
      let l = List.rev acc in
      begin match parse_expr env f with
        | Term t -> Term (mk_let env ast T.letin l t)
        | res -> _expected env "term of formula" f (Some res)
      end

  and parse_let_seq env ast acc f = function
    | [] ->
      let[@inline] aux t = parse_let_seq_end env ast acc t in
      (wrap_attr[@inlined]) apply_attr env f aux
    | x :: r ->
      begin match x with
        | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s; _ } as w, e); _ }
        | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq; _}, [
                { Ast.term = Ast.Symbol s; _ } as w; e]); _ }
        | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv; _}, [
                { Ast.term = Ast.Symbol s; _ } as w; e]); _ } ->
          let t = parse_term env e in
          let v = mk_term_var env (Id.name s) (T.ty t) in
          let env' = bind_term_var env s e v t w in
          parse_let_seq env' ast ((v, t) :: acc) f r
        | t -> _expected env "variable binding" t None
      end

  and parse_let_par env ast acc f = function
    | [] ->
      let env, rev_l =
        List.fold_right (fun (s, e, v, t, w) (env, acc) ->
            let env' = bind_term_var env s e v t w in
            (env', (v, t) :: acc)
          ) acc (env, [])
      in
      let l = List.rev rev_l in
      begin match parse_expr env f with
        | Term t -> Term (mk_let env ast T.letand l t)
        | res -> _expected env "term of formula" f (Some res)
      end
    | x :: r ->
      begin match x with
        | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s; _ } as w, e); _ }
        | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq; _}, [
                { Ast.term = Ast.Symbol s; _ } as w; e]); _ }
        | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv; _}, [
                { Ast.term = Ast.Symbol s; _ } as w; e]); _ } ->
          begin match parse_term env e with
            | t ->
              let v = mk_term_var env (Id.name s) (T.ty t) in
              parse_let_par env ast ((s, e, v, t, w) :: acc) f r
            (* Try and provide a helpful hints when a parallel let is used as
               a sequential let-binding *)
            | exception (Typing_error (Error (
                env, fragment, Cannot_find (id, ""))))
                when List.exists (fun (s, _, _, _, _) -> Id.equal s id) acc ->
              let msg =
                "This binding occurs in a parallel let-binding; you cannot refer \
                 to other variables defined by the same let-binding in the defining \
                 expressions."
              in
              _error env fragment (Cannot_find (id, msg))
          end
        | t -> _expected env "variable binding" t None
      end

  and parse_adt_cstr env ast =
    match ast with
    | { Ast.term = Ast.Symbol s; _ }
    | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, []); _} ->
      begin match find_bound env s with
        | `Cstr c -> c
        | `Not_found -> _cannot_find env ast s
        | _ -> _expected env "adt constructor" ast None
      end
    | _ -> _expected env "adt constructor name" ast None

  and parse_adt_check env ast = function
    | [ adt_ast; cstr_ast ] ->
      let adt = parse_term env adt_ast in
      let c = parse_adt_cstr env cstr_ast in
      Term (_wrap2 env ast T.cstr_tester c adt)
    | l ->
      _bad_op_arity env (Builtin Ast.Adt_check) 2 (List.length l) ast

  and parse_adt_project env ast = function
    | [ adt_ast; dstr_ast ] ->
      begin match dstr_ast with
        | { Ast.term = Ast.Symbol s; _ }
        | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, []); _} ->
          begin match find_bound env s with
            | `Dstr d ->
              parse_app_term_cst env ast d [adt_ast]
            (* field access for records can be seen as a special case of adt
               projection (the same way record type definition are a special
               case of adt type definition with a single case). *)
            | `Field field ->
              let t = parse_term env adt_ast in
              Term (create_record_access env ast t field)
            (* error cases *)
            | `Not_found -> _cannot_find env ast s
            | _ -> _expected env "adt destructor/field" ast None
          end
        | _ -> _expected env "adt destructor/field name" ast None
      end
    | l ->
      _bad_op_arity env (Builtin Ast.Adt_project) 2 (List.length l) ast

  and parse_record_field env ast =
    match ast with
    | { Ast.term = Ast.Symbol s; _ }
    | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, []); _} ->
      begin match find_bound env s with
        | `Field f -> f
        | `Not_found -> _cannot_find env ast s
        | _ -> _expected env "record field" ast None
      end
    | _ ->
      _expected env "record field name" ast None

  and parse_record_field_binding env ast =
    match ast with
    | { Ast.term = Ast.App (
        {Ast.term = Ast.Builtin Ast.Eq; _ }, [field; value] ); _ } ->
      let f = parse_record_field env field in
      let t = parse_term env value in
      f, t
    | _ ->
      _expected env "record field_binding" ast None

  and parse_record env ast = function
    | [] ->
      _expected env "at least one field binding" ast None
    | l ->
      let l' = List.map (parse_record_field_binding env) l in
      Term (create_record env ast l')

  and parse_record_with env ast = function
    | [] ->
      _expected env "term" ast None
    | t :: l ->
      let t' = parse_term env t in
      let l' = List.map (parse_record_field_binding env) l in
      Term (create_record_with env ast t' l')

  and parse_record_access env ast = function
    | [ t; f ] ->
      let t = parse_term env t in
      let field = parse_record_field env f in
      Term (create_record_access env ast t field)
    | l ->
      _bad_op_arity env (Builtin Ast.Record_access) 2 (List.length l) ast

  and parse_symbol env ast s s_ast =
    parse_app_symbol env ast s s_ast []

  and parse_app env ast f_ast args_asts =
    let[@inline] aux t = parse_app_aux env ast args_asts t in
    (wrap_attr[@inlined]) apply_attr env f_ast aux

  and parse_app_aux env ast args_asts = function
    | { Ast.term = Ast.App (g, inner_args); _ } ->
      parse_app env ast g (inner_args @ args_asts)
    | { Ast.term = Ast.Symbol s; _ } as f_ast ->
      parse_app_symbol env ast s f_ast args_asts
    | { Ast.term = Ast.Builtin b; _ } ->
      parse_app_builtin env ast b args_asts
    | f_ast -> parse_app_ho env ast f_ast args_asts

  and parse_app_ho env ast f_ast args_asts =
    match env.order with
    | First_order ->
      _error env (Ast ast) Higher_order_application
    | Higher_order ->
      let f = parse_expr env f_ast in
      parse_app_ho_generic env ast f f_ast args_asts

  and parse_app_ho_generic env ast f f_ast args =
    match f with
    | Ttype | Ty _ | Tags _ -> _expected env "a term" f_ast (Some f)
    | Term f -> parse_app_ho_term env ast f args

  and parse_app_ho_term env ast f args =
    let n_ty =
      match Ty.view (T.ty f) with
      | `Pi (vars, _) -> List.length vars
      | _ -> 0
    in
    let args = List.map (fun ast -> ast, parse_expr env ast) args in
    let ty_args, t_args = split_ho_args env ast n_ty args in
    Term (_wrap3 env ast T.apply f ty_args t_args)

  and parse_app_symbol env ast s s_ast args =
    parse_app_resolved env ast s s_ast args (find_bound env s)

  and parse_app_resolved env ast s s_ast args = function
    | `Ty_var v -> parse_app_ty_var env ast v s_ast args
    | `Term_var v -> parse_app_term_var env ast v s_ast args
    | `Letin (_, _, v, t) -> parse_app_letin_var env ast v s_ast t args
    | `Ty_cst f -> parse_app_ty_cst env ast f args
    | `Term_cst f -> parse_app_term_cst env ast f args
    | `Dstr c -> parse_app_term_cst env ast c args
    | `Cstr c ->
      parse_app_cstr env ast c args
    | `Field _f ->
      _expected env "not a field name" s_ast None
    | `Builtin b ->
      builtin_apply_id env b ast s s_ast args
    | `Not_found ->
      infer_sym env ast s args s_ast

  and builtin_apply_id env b ast s s_ast args : res =
    match (b : builtin_res) with
    | #builtin_common as b -> builtin_apply_common env b ast args
    | `Infer (_reason, var_infer, sym_infer) ->
      infer_sym_aux env var_infer sym_infer ast s args s_ast
    | `Reserved (_reason, (`Solver | `Term_cst _ )) ->
      (* reserved builtins are there to provide shadow warnings
         and provide symbols for model definitions, but they don't
         have a semantic outside of that. *)
      _cannot_find env ast s

  and parse_app_ty_var env ast v _v_ast args =
    mark_ty_var_as_used v;
    if args = [] then Ty (Ty.of_var v)
    else _ty_var_app env v ast

  and parse_app_term_var env ast v v_ast args =
    mark_term_var_as_used v;
    match env.order with
    | First_order ->
      if args = [] then Term (T.of_var v)
      else _var_app env v ast
    | Higher_order ->
      parse_app_ho_generic env ast (Term (T.of_var v)) v_ast args

  and parse_app_letin_var env ast v v_ast t args =
    mark_term_var_as_used v;
    match env.order with
    | First_order ->
      if args = [] then Term t
      else _var_app env v ast
    | Higher_order ->
      parse_app_ho_generic env ast (Term t) v_ast args

  and parse_app_ty_cst env ast f args =
    if List.length args <> Ty.Const.arity f then
      _bad_ty_arity env f (List.length args) ast;
    let l = List.map (parse_ty env) args in
    Ty (Ty.apply f l)

  and parse_app_term_cst env ast f args =
    match env.order with
    | First_order ->
      let n_ty, n_t = arity (T.Const.ty f) in
      let ty_args, t_l =
        match split_fo_args env ast n_ty n_t args with
        | `Ok (l, l') ->
          let ty_args = List.map (parse_ty env) l in
          ty_args, l'
        | `Fixed (l, l') -> l, l'
        | `Bad_arity (expected, actual) ->
          _bad_term_arity env f expected actual ast
      in
      let t_args = List.map (parse_term env) t_l in
      Term (_wrap3 env ast T.apply_cst f ty_args t_args)
    | Higher_order ->
      let n_ty, _ = arity (T.Const.ty f) in
      let args = List.map (fun ast -> ast, parse_expr env ast) args in
      let ty_args, t_args = split_ho_args env ast n_ty args in
      Term (_wrap3 env ast T.apply_cst f ty_args t_args)

  and parse_app_cstr env ast c args =
    let n_ty, n_t = arity (T.Cstr.ty c) in
    let ty_args, t_l =
      match split_fo_args env ast n_ty n_t args with
      | `Ok (l, l') ->
        let ty_args = List.map (parse_ty env) l in
        ty_args, l'
      | `Fixed (l, l') -> l, l'
      | `Bad_arity (expected, actual) ->
        _bad_cstr_arity env c expected actual ast
    in
    let t_args = List.map (parse_term env) t_l in
    Term (_wrap3 env ast T.apply_cstr c ty_args t_args)

  and parse_app_builtin env ast b args =
    match env.builtins env (Builtin b) with
    | `Not_found -> _unknown_builtin env ast b
    | #builtin_res as b_res -> builtin_apply_builtin env ast b b_res args

  and builtin_apply_common env b ast args =
    match (b : builtin_common) with
    | `Ttype (_meta, f) -> _wrap2 env ast f ast args; Ttype
    | `Ty (_meta, f) -> Ty (_wrap2 env ast f ast args)
    | `Term (_meta, f) -> Term (_wrap2 env ast f ast args)
    | `Tags (_meta, f) -> Tags (_wrap2 env ast f ast args)

  and builtin_apply_builtin env ast b b_res args : res =
    match (b_res : builtin_res) with
    | #builtin_common as b -> builtin_apply_common env b ast args
    | `Reserved (_reason, (`Solver | `Term_cst _)) -> _unknown_builtin env ast b
    | `Infer _ ->
      (* TODO: proper erorr.
         We do not have a map from builtins symbols to typed expressions. *)
      assert false

  and parse_builtin env ast b =
    parse_app_builtin env ast b []

  and parse_ensure env ast expected =
    let t = parse_term env ast in
    let ty = parse_ty env expected in
    Term (_wrap2 env ast T.ensure t ty)

  and parse_ty env ast =
    unwrap_ty env ast (parse_expr (expect_type env) ast)

  and parse_term env ast =
    unwrap_term env ast (parse_expr (expect_term env) ast)

  and parse_prop env ast =
    match parse_expr (expect_term env) ast with
    | Term t -> _wrap2 env ast T.ensure t Ty.prop
    | res -> _expected env "term/prop" ast (Some res)

  and infer_ty env ast src shape args =
    match env.order with
    | Higher_order -> wildcard env src shape
    | First_order ->
      begin match shape with
        | Arrow { arg_shape; ret_shape; } ->
          let arg_src = Arg_of src in
          let ty_args = List.map (infer_ty_arg env ast arg_src arg_shape) args in
          let ty_ret = infer_ty env ast (Ret_of src) ret_shape [] in
          Ty.arrow ty_args ty_ret
        | _ -> wildcard env src shape
      end

  and infer_ty_arg env ast src shape arg =
    let ty = T.ty arg in
    match Ty.view ty with
    | `Wildcard w ->
      begin match shape with
        | Any_base { allowed = [ty]; preferred = _; } ->
          _wrap2 env ast Ty.set_wildcard w ty;
          ty
        | _ ->
          let mark = { shape; src; bound = env.type_locs; } in
          transfer_hook env.wildcards mark w;
          ty
      end
    | _ -> infer_ty env ast src shape []

  and infer_var_in_binding_pos env ast s =
    match env.expect with
    | Anything -> _cannot_infer_var_in_binding_pos env ast
    | Type ->
      if not env.var_infer.infer_type_vars_in_binding_pos then
        _cannot_infer_var_in_binding_pos env ast
      else begin
        let v = mk_ty_var env (Id.name s) in
        env.var_infer.var_hook (`Ty_var v);
        `Ty (s, v)
      end
    | Term ->
      begin match env.var_infer.infer_term_vars_in_binding_pos with
        | No_inference -> _cannot_infer_var_in_binding_pos env ast
        | Wildcard shape ->
          let var_infer = {
              variable = s;
              variable_loc = ast.loc;
              inferred_ty = Ty.prop;
            } in
          let ty = infer_ty env ast (Variable_inference var_infer) shape [] in
          var_infer.inferred_ty <- ty;
          let v = mk_term_var env (Id.name s) ty in
          env.var_infer.var_hook (`Term_var v);
          `Term (s, v)
      end

  and infer_sym env ast s args s_ast =
    infer_sym_aux env env.var_infer env.sym_infer ast s args s_ast

  and infer_sym_aux env var_infer sym_infer ast s args s_ast =
    (* variables must be bound explicitly *)
    if Id.(s.ns = Var) then begin
      match var_infer.infer_unbound_vars with
      | No_inference -> _cannot_find env s_ast s
      | Unification_type_variable ->
        let v = wildcard_var env (From_source s_ast) Any_in_scope in
        add_inferred_type_var env s v s_ast;
        var_infer.var_hook (`Ty_var v);
        parse_app_ty_var env ast v s_ast args
    end else
      match env.expect with
      | Anything -> _cannot_find env s_ast s
      | Type ->
        if not sym_infer.infer_type_csts
        then _cannot_find env s_ast s
        else begin
          let n = List.length args in
          let f = mk_ty_cst env (Id.name s) n in
          decl_ty_const env (Ast ast) s f (Inferred (env.file, s_ast));
          sym_infer.sym_hook (`Ty_cst f);
          parse_app_ty_cst env ast f args
        end
      | Term ->
        begin match sym_infer.infer_term_csts with
          | No_inference -> _cannot_find env s_ast s
          | Wildcard shape ->
            let t_args = List.map (parse_term env) args in
            let f =
              match find_bound env s with
              | `Term_cst f -> f
              | `Not_found | `Builtin `Infer _ ->
                let sym = {
                  symbol = s;
                  symbol_loc = s_ast.Ast.loc;
                  inferred_ty = Ty.prop;
                } in
                let src = Symbol_inference sym in
                let f_ty = infer_ty env ast src shape t_args in
                sym.inferred_ty <- f_ty;
                let f = mk_term_cst env (Id.name s) f_ty in
                decl_term_const env (Ast ast) s f (Inferred (env.file, s_ast));
                sym_infer.sym_hook (`Term_cst f);
                f
              | _ -> assert false
            in
            Term (_wrap3 env ast T.apply_cst f [] t_args)
        end

  let parse_ttype_var_in_binding_pos env t =
    match parse_var_in_binding_pos (expect_type env) t with
    | `Ty (id, v) -> (id, v, t)
    | `Term (_, v) ->
      _expected env "type variable" t (Some (Term (T.of_var v)))

  let parse_typed_var_in_binding_pos env t =
    match parse_var_in_binding_pos (expect_term env) t with
    | `Term (id, v) -> (id, v, t)
    | `Ty (_, v) ->
      _expected env "typed variable" t (Some (Ty (Ty.of_var v)))

  let rec parse_sig_quant env = function
    | { Ast.term = Ast.Binder (Ast.Pi, vars, t); _ } ->
      let ttype_vars = List.map (parse_ttype_var_in_binding_pos env) vars in
      let ttype_vars, env' = add_type_vars env ttype_vars in
      let l = List.combine vars ttype_vars in
      parse_sig_arrow l [] env' t
    | t ->
      parse_sig_arrow [] [] env t

  and parse_sig_arrow ttype_vars (ty_args: (Ast.t * res) list) env = function
    | { Ast.term = Ast.Binder (Ast.Arrow, args, ret); _ } ->
      let t_args = parse_sig_args env args in
      parse_sig_arrow ttype_vars (ty_args @ t_args) env ret
    | t ->
      begin match parse_expr env t with
        | Ttype ->
          let n = List.length ttype_vars in
          let aux n arg =
            match (arg : _ * res) with
            | (_, Ttype) -> n + 1
            | (ast, res) -> raise (Found (ast, res))
          in
          begin
            match List.fold_left aux n ty_args with
            | n -> `Ty_cstr n
            | exception Found (ast, res) ->
              _expected env "tType or a type variable" ast (Some res)
          end
        | Ty ret ->
          let aux acc arg =
            match (arg : _ * res) with
            | (_, Ty t) -> t :: acc
            | (ast, res) -> raise (Found (ast, res))
          in
          begin
            match List.fold_left aux [] ty_args with
            | exception Found (err, res) -> _expected env "type" err (Some res)
            | l -> `Fun_ty (List.map snd ttype_vars, List.rev l, ret)
          end
        | res -> _expected env "Ttype of type" t (Some res)
      end

  and parse_sig_args env l =
    List.flatten @@ List.map (parse_sig_arg env) l

  and parse_sig_arg env = function
    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Product; _}, l); _ } ->
      List.flatten @@ List.map (parse_sig_arg env) l
    | t ->
      [t, parse_expr env t]

  let parse_sig = parse_sig_quant

  let parse_inductive_arg env = function
    | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s; _ }, e); _ } ->
      let ty = parse_ty env e in
      ty, Some s
    | t ->
      let ty = parse_ty env t in
      ty, None


  (* Typechecking mutually recursive datatypes *)
  (* ************************************************************************ *)

  let decl_id t =
    match (t : Stmt.decl) with
    | Abstract { id; _ }
    | Record { id; _ }
    | Inductive { id; _ } -> id

  let appears_in s t =
    let mapper =
      { Ast.unit_mapper with
        symbol = (fun _ ~attr:_ ~loc:_ id ->
            if Id.equal s id then raise Exit);
      }
    in
    try Ast.map mapper t; false
    with Exit -> true

  let well_founded_aux l t =
    match (t : Stmt.decl) with
    | Abstract _ -> true
    | Inductive { cstrs; _ } ->
      List.exists (fun (_, args) ->
          List.for_all (fun t ->
              not (List.exists (fun i ->
                  appears_in (decl_id i) t
                ) l)
            ) args
        ) cstrs
    | Record { fields; _ } ->
      List.for_all (fun (_, t) ->
          not (List.exists (fun i ->
              appears_in (decl_id i) t
            ) l)
        ) fields

  let rec check_well_founded env d l =
    match (l : Stmt.decl list) with
    | [] -> ()
    | _ ->
      let has_progressed = ref false in
      let l' = List.filter (fun t ->
          let b = well_founded_aux l t in
          if b then has_progressed := true;
          not b
        ) l in
      if !has_progressed then
        check_well_founded env d l'
      else
        _error env (Decls d) (Not_well_founded_datatypes l')

  let record env d ty_cst { Stmt.vars; fields; _ } =
    let ttype_vars = List.map (parse_ttype_var_in_binding_pos env) vars in
    let ty_vars, env = add_type_vars env ttype_vars in
    let l = List.map (fun (id, t) ->
        let ty = parse_ty env t in
        check_no_free_wildcards env t;
        cst_path env (Id.name id), ty
      ) fields in
    let def, field_list = T.define_record ty_cst ty_vars l in
    List.iter2 (fun (id, _) field ->
        decl_term_field env (Decl d) id field (Declared (env.file, d))
      ) fields field_list;
    def

  let inductive env d ty_cst { Stmt.id; vars; cstrs; _ } =
    (* Parse the type variables *)
    let ttype_vars = List.map (parse_ttype_var_in_binding_pos env) vars in
    let ty_vars, env = add_type_vars env ttype_vars in
    (* Parse the constructors *)
    let cstrs_with_ids = List.map (fun (id, args) ->
        id, List.map (fun t ->
            let ty, dstr = parse_inductive_arg env t in
            check_no_free_wildcards env t;
            t, ty, dstr
          ) args
      ) cstrs in
    (* Constructors with strings for names *)
    let cstrs_with_strings = List.map (fun (id, args) ->
        cst_path env (Id.name id), List.map (fun (_, ty, dstr) ->
            ty, Misc.Options.map (fun id -> cst_path env (Id.name id)) dstr
          ) args
      ) cstrs_with_ids in
    (* Call the T module to define the adt and get the typed constructors
       and destructors. *)
    let def, defined_cstrs = T.define_adt ty_cst ty_vars cstrs_with_strings in
    (* Register the constructors and destructors in the global env. *)
    List.iter2 (fun (cid, pargs) (c, targs) ->
        decl_term_cstr env (Decl d) cid c (Declared (env.file, d));
        List.iter2 (fun (t, _, dstr) (_, o) ->
            match dstr, o with
            | None, None -> ()
            | None, Some c ->
              _warn env (Ast t) (Superfluous_destructor (id, cid, c))
            | Some id, Some const ->
              decl_term_dstr env (Decl d) id const (Declared (env.file, d))
            | Some id, None ->
              _error env (Ast t) (Missing_destructor id)
          ) pargs targs
      ) cstrs_with_ids defined_cstrs;
    def

  let define_decl env (_, cst) t =
    match cst, (t : Stmt.decl) with
    (* Term decl *)
    | (`Term_decl _) as res, Abstract _ -> res
    (* Abstract type decl *)
    | `Type_decl c, Abstract _ -> `Type_decl (c, None)
    (* ADT type defs *)
    | `Term_decl _, Inductive _ -> assert false
    | `Type_decl c, Inductive i ->
      `Type_decl (c, Some (inductive env t c i))
    (* Record type defs *)
    | `Term_decl _, Record _ -> assert false
    | `Type_decl c, Record r ->
      `Type_decl (c, Some (record env t c r))

  let parse_decl tags env (t : Stmt.decl) =
    match t with
    | Abstract { id; ty = ast; loc = _; attrs; } ->
      let tags = tags @ parse_attrs env [] attrs in
      begin match parse_sig env ast with
        | `Ty_cstr n ->
          check_no_free_wildcards env ast;
          let c = mk_ty_cst env (Id.name id) n in
          List.iter (function
              | Set (tag, v) -> Ty.Const.set_tag c tag v
              | Add (tag, v) -> Ty.Const.add_tag c tag v
            ) tags;
          env, (id, `Type_decl c)
        | `Fun_ty (vars, args, ret) ->
          let ty = Ty.pi vars (Ty.arrow args ret) in
          let env, ty = finalize_wildcards_ty (`Decl t) env ast ty in
          let f = mk_term_cst env (Id.name id) ty in
          List.iter (function
              | Set (tag, v) -> T.Const.set_tag f tag v
              | Add (tag, v) -> T.Const.add_tag f tag v
            ) tags;
          env, (id, `Term_decl f)
      end
    | Record { id; vars; fields = _; loc = _; attrs }
    | Inductive { id; vars; cstrs = _; loc = _; attrs } ->
      let tags = tags @ parse_attrs env [] attrs in
      let n = List.length vars in
      let c = mk_ty_cst env (Id.name id) n in
      List.iter (function
          | Set (tag, v) -> Ty.Const.set_tag c tag v
          | Add (tag, v) -> Ty.Const.add_tag c tag v
        ) tags;
      env, (id, `Type_decl c)

  let record_decl env (id, tdecl) (t : Stmt.decl) =
    match tdecl with
    | `Type_decl c -> decl_ty_const env (Decl t) id c (Declared (env.file, t))
    | `Term_decl f -> decl_term_const env (Decl t) id f (Declared (env.file, t))

  let decls env ?(attrs=[]) (d: Stmt.decl Stmt.group) =
    let tags =
      List.flatten @@ List.map (fun ast ->
        let l = parse_attr env ast in
        check_no_free_wildcards env ast;
        l
        ) attrs
    in
    if d.recursive then begin
      (* Check well-foundedness *)
      check_well_founded env d d.contents;
      (* First pre-parse all definitions and generate the typed symbols for them *)
      let env, parsed = Misc.Lists.fold_left_map (parse_decl tags) env d.contents in
      (* Then, since the decls are recursive, register in the global env the type
         const for each decl before fully parsing and defining them. *)
      let () = List.iter2 (record_decl env) parsed d.contents in
      (* Then parse the complete type definition and define them.
         TODO: parse (and thus define them with T) in the topological order
             defined by the well-founded check ? *)
      let defs = List.map2 (define_decl env) parsed d.contents in
      (* Return the defined types *)
      defs
    end else begin
      List.map (fun t ->
          (* First pre-parse all definitions and generate the typed symbols for them *)
          let env, parsed = parse_decl tags env t in
          (* Then parse the complete type definition and define them. *)
          let def = define_decl env parsed t in
          (* Finally record them in the state *)
          let () = record_decl env parsed t in
          (* return *)
          def
        ) d.contents
    end

  (* Definitions *)
  (* ************************************************************************ *)

  let parse_def_vars env vars =
    let rec aux env acc = function
      | [] -> env, List.rev acc
      | v :: r ->
        let id, v, ast = parse_ttype_var_in_binding_pos env v in
        let env = add_type_var env id v ast in
        aux env (v :: acc) r
    in
    aux env [] vars

  let parse_def_params env params =
    let rec aux env acc = function
      | [] -> env, List.rev acc
      | p :: r ->
        let id, v, ast = parse_typed_var_in_binding_pos env p in
        let env = add_term_var env id v ast in
        aux env (v :: acc) r
    in
    aux env [] params

  let parse_def_sig env (d: Stmt.def) =
    let env, vars = parse_def_vars env d.vars in
    let env, params = parse_def_params env d.params in
    match parse_expr env d.ret_ty with
    | Ttype ->
      begin match params with
        | [] ->
          env, vars, [], `Ty_def
        | _ :: _ ->
          _expected env "non_dependant type (or a term)" d.ret_ty None
      end
    | Ty ret_ty ->
      env, vars, params, `Term_def ret_ty
    | (Term _ as res)
    | (Tags _ as res) ->
      _expected env "ttype or a type" d.ret_ty (Some res)

  let close_wildcards_in_sig (env, vars, params, ssig) (d : Stmt.def) =
    let env, l = finalize_wildcards_def (`Def d) env d.ret_ty in
    env, l @ vars, params, ssig

  let create_id_for_def ~freshen ~defs tags (env, vars, params, ssig) (d: Stmt.def) =
    let tags = tags @ parse_attrs env [] d.attrs in
    match ssig with
    | `Ty_def ->
      assert (params = []);
      let c = mk_ty_cst env (Id.name d.id) (List.length vars) in
      List.iter (function
          | Set (tag, v) -> Ty.Const.set_tag c tag v
          | Add (tag, v) -> Ty.Const.add_tag c tag v
        ) tags;
      if defs.Stmt.recursive
      then _error env (Defs defs) (Type_def_rec d)
      else decl_ty_const env (Def d) d.id c (Defined (env.file, d));
      `Ty (d.id, c)
    | `Term_def ret_ty ->
      let params_tys = List.map (fun p -> T.Var.ty p) params in
      let ty = Ty.pi vars (Ty.arrow params_tys ret_ty) in
      let ty = if freshen then Ty.freshen ty else ty in
      let f = mk_term_cst env (Id.name d.id) ty in
      List.iter (function
          | Set (tag, v) -> T.Const.set_tag f tag v
          | Add (tag, v) -> T.Const.add_tag f tag v
        ) tags;
      decl_term_const env (Def d) d.id f (Defined (env.file, d));
      `Term (d.id, f)

  let lookup_id_for_def_ty env (d : Stmt.def) vars params cst reason =
    assert (params = []);
    let n_vars = List.length vars in
    let n = Ty.Const.arity cst in
    if n = n_vars then `Ty (d.id, cst)
    else _incoherent_type_redefinition env d cst reason n_vars

  let lookup_id_for_def_term env (d : Stmt.def) vars params ret_ty cst reason =
    let params_ty = List.map T.Var.ty params in
    let ty = Ty.pi vars (Ty.arrow params_ty ret_ty) in
    if Ty.equal ty (T.Const.ty cst) then `Term (d.id, cst)
    else begin
      match Ty.instance_of (T.Const.ty cst) ty with
      | None -> _incoherent_term_redefinition env d cst reason ty
      | Some ty_args -> `Instanceof (d.id, cst, ty_args)
    end

  let lookup_id_for_def _ (env, vars, params, ssig) (d: Stmt.def) =
    match ssig, find_bound env d.id with
    (* Type definitions *)
    | `Ty_def, ((`Ty_cst cst) as c) ->
      begin match find_reason env c with
        | Some (Declared _ as reason) ->
          lookup_id_for_def_ty env d vars params cst reason
        | reason -> _id_def_conflict env d.loc d.id (with_reason reason c)
      end

    (* Term definitions *)
    | `Term_def ret_ty, (`Dstr cst as bound) ->
      begin match find_reason env bound with
        | Some reason ->
          lookup_id_for_def_term env d vars params ret_ty cst reason
        | None ->
          assert false (* missing reason for destructor *)
      end
    | `Term_def ret_ty, `Builtin `Term (`Partial mk_cst, _)
    | `Term_def ret_ty, `Builtin `Reserved (_, `Term_cst mk_cst) ->
      let cst = mk_cst vars params ret_ty in
      lookup_id_for_def_term env d vars params ret_ty cst Builtin
    | `Term_def ret_ty, ((`Term_cst cst) as c) ->
      begin match find_reason env c with
        | Some (Declared _ as reason) ->
          lookup_id_for_def_term env d vars params ret_ty cst reason
        | reason -> _id_def_conflict env d.loc d.id (with_reason reason c)
      end

    (* Error cases *)
    | _, `Not_found ->
      _id_def_conflict env d.loc d.id `Not_found
    | _, (#bound as bound) ->
      _id_def_conflict env d.loc d.id (with_reason (find_reason env bound) bound)


  let id_for_def ~freshen ~mode ~defs tags ssig d =
    match mode with
    | `Create_id -> create_id_for_def ~freshen ~defs tags ssig d
    | `Use_declared_id -> lookup_id_for_def tags ssig d

  let parse_def (env, _vars, _params, ssig) (d : Stmt.def) =
    match ssig, parse_expr env d.body with
    | `Ty_def, Ty body -> d.body, `Ty body
    | `Term_def ret_ty, Term body ->
      d.body, `Term (_wrap2 env d.body T.ensure body ret_ty)
    | _, ((Ttype | Tags _) as ret) ->
      _expected env "term or a type" d.body (Some ret)
    | _ -> assert false

  let finalize_def id (env, vars, params, _ssig) (ast, ret) =
    check_no_free_wildcards env ast;
    match id, ret with
    (* type alias *)
    | `Ty (id, c), `Ty body ->
      assert (params = []);
      List.iter (check_used_ty_var ~kind:`Type_alias_param env) vars;
      `Type_alias (id, c, vars, body)
    (* function definition *)
    | `Term (id, f), `Term body ->
      List.iter (check_used_ty_var ~kind:`Function_param env) vars;
      List.iter (check_used_term_var ~kind:`Function_param env) params;
      `Term_def (id, f, vars, params, body)
    | `Instanceof (id, f, ty_args), `Term body ->
      List.iter (check_used_ty_var ~kind:`Function_param env) vars;
      List.iter (check_used_term_var ~kind:`Function_param env) params;
      `Instanceof (id, f, ty_args, vars, params, body)
    (* error cases *)
    | `Ty _, `Term _
    | (`Term _ | `Instanceof _), `Ty _ -> assert false

  let defs ?(mode=`Create_id) env ?(attrs=[]) (d : Stmt.defs) =
    let tags = parse_attrs env [] attrs in
    if d.recursive then begin
      let envs = List.map (fun _ -> split_env_for_def env) d.contents in
      let sigs = List.map2 parse_def_sig envs d.contents in
      let sigs = List.map2 close_wildcards_in_sig sigs d.contents in
      let ids = List.map2 (id_for_def ~freshen:true ~defs:d ~mode tags) sigs d.contents in
      let defs = List.map2 parse_def sigs d.contents in
      Misc.Lists.map3 finalize_def ids sigs defs
    end else begin
      List.map (fun t ->
          let env = split_env_for_def env in
          let ssig = parse_def_sig env t in
          let def = parse_def ssig t in
          let ssig = close_wildcards_in_sig ssig t in
          let id = id_for_def ~freshen:false ~defs:d ~mode tags ssig t in
          finalize_def id ssig def
        ) d.contents
    end

  (* High-level parsing function *)
  (* ************************************************************************ *)

  let parse env ast =
    let res = parse_prop env ast in
    let _env, res = finalize_wildcards_prop (`Term ast) env ast res in
    res

end
