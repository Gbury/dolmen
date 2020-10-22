
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = Tff_intf.S

(* Typechecking functor *)
(* ************************************************************************ *)

module Make
    (Tag: Dolmen.Intf.Tag.S)
    (Ty: Dolmen.Intf.Ty.Tff
     with type 'a tag := 'a Tag.t)
    (T: Dolmen.Intf.Term.Tff
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type 'a tag := 'a Tag.t)
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

  (* Different behavior of polymorphism *)
  type poly =
    | Explicit
    | Implicit
    | Flexible

  (* The type of potentially expected result type for parsing an expression *)
  type expect =
    | Nothing
    | Type
    | Typed of Ty.t

  (* The type returned after parsing an expression. *)
  type tag =
    | Any : 'a Tag.t * 'a -> tag

  (* Result of parsing an expression *)
  type res =
    | Ttype
    | Ty    of Ty.t
    | Term  of T.t
    | Tags  of tag list


  (* Things that can be inferred *)
  type inferred =
    | Ty_fun of Ty.Const.t
    | Term_fun of T.Const.t

  (* Wrapper around potential function symbols in Dolmen *)
  type symbol =
    | Id of Id.t
    | Builtin of Ast.builtin

  (* Not found result *)
  type not_found = [ `Not_found ]

  (* Variable that can be bound to a dolmen identifier *)
  type var = [
    | `Ty_var of Ty.Var.t
    | `Term_var of T.Var.t
    | `Letin of Ast.t * T.Var.t * T.t
  ]

  (* Constants that can be bound to a dolmen identifier. *)
  type cst = [
    | `Cstr of T.Cstr.t
    | `Field of T.Field.t
    | `Ty_cst of Ty.Const.t
    | `Term_cst of T.Const.t
  ]

  (* Result of parsing a symbol by the theory *)
  type builtin_res = [
    | `Ttype of (Ast.t -> Ast.t list -> unit)
    | `Ty    of (Ast.t -> Ast.t list -> Ty.t)
    | `Term  of (Ast.t -> Ast.t list -> T.t)
    | `Tags  of (Ast.t -> Ast.t list -> tag list)
  ]

  (* Names that are bound to a dolmen identifier by the builtins *)
  type builtin = [
    | `Builtin of builtin_res
  ]

  (* Either a bound variable or a bound constant *)
  type bound = [ var | cst | builtin ]

  type reason =
    | Builtin
    | Bound of Loc.file * Ast.t
    | Inferred of Loc.file * Ast.t
    | Defined of Loc.file * Stmt.def
    | Declared of Loc.file * Stmt.decl
  (** The type of reasons for constant typing *)

  type binding = [
    | `Not_found
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
        | `Term of T.Const.t * reason option
        | `Field of T.Field.t * reason option
      ]
  ]
  (** The bindings that can occur. *)

  (* Maps & Hashtbls *)
  (* ************************************************************************ *)

  module M = Map.Make(Id)

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
    | Unused_type_variable : Ty.Var.t -> Ast.t warn
    (* Unused quantified type variable *)
    | Unused_term_variable : T.Var.t -> Ast.t warn
    (* Unused quantified term variable *)
    | Error_in_attribute : exn -> Ast.t warn
    (* An error occurred wile parsing an attribute *)
    | Superfluous_destructor : Id.t * Id.t * T.Const.t -> Ast.t warn
    (* The user implementation of typed terms returned a destructor where
       was asked for. This warning can very safely be ignored. *)

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
    | Infer_type_variable : Ast.t err
    | Expected : string * res option -> Ast.t err
    | Bad_index_arity : string * int * int -> Ast.t err
    | Bad_ty_arity : Ty.Const.t * int -> Ast.t err
    | Bad_op_arity : string * int list * int -> Ast.t err
    | Bad_cstr_arity : T.Cstr.t * int list * int -> Ast.t err
    | Bad_term_arity : T.Const.t * int list * int -> Ast.t err
    | Repeated_record_field : T.Field.t -> Ast.t err
    | Missing_record_field : T.Field.t -> Ast.t err
    | Mismatch_record_type : T.Field.t * Ty.Const.t -> Ast.t err
    | Var_application : T.Var.t -> Ast.t err
    | Ty_var_application : Ty.Var.t -> Ast.t err
    | Type_mismatch : T.t * Ty.t -> Ast.t err
    | Quantified_var_inference : Ast.t err
    | Unhandled_builtin : Ast.builtin -> Ast.t err
    | Cannot_tag_tag : Ast.t err
    | Cannot_tag_ttype : Ast.t err
    | Cannot_find : Id.t -> Ast.t err
    | Type_var_in_type_constructor : Ast.t err
    | Forbidden_quantifier : Ast.t err
    | Missing_destructor : Id.t -> Ast.t err
    | Type_def_rec : Stmt.def -> Stmt.defs err
    | Higher_order_application : Ast.t err
    | Higher_order_type : Ast.t err
    | Unbound_variables : Ty.Var.t list * T.Var.t list * T.t -> Ast.t err
    | Uncaught_exn : exn * Printexc.raw_backtrace -> Ast.t err
    | Unhandled_ast : Ast.t err


  (* State & Environment *)
  (* ************************************************************************ *)

  (* Global, mutable state. *)
  type state = {

    mutable csts : cst M.t;
    (* association between dolmen ids and types/terms constants. *)

    mutable ttype_locs : reason R.t;
    (* stores reasons for typing of type constructors *)
    mutable const_locs : reason S.t;
    (* stores reasons for typing of constants *)
    mutable cstrs_locs : reason U.t;
    (* stores reasons for typing constructors *)
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

    (* Map from term variables to the reason of its type *)
    type_locs : reason E.t;
    term_locs : reason F.t;

    (* bound variables *)
    vars : var M.t;

    (* The current builtin symbols *)
    builtins : builtin_symbols;

    (* warnings *)
    warnings : warning -> unit;

    (* Additional typing info *)
    poly         : poly;
    quants       : bool;

    expect       : expect;
    infer_base   : Ty.t option;
    infer_hook   : env -> inferred -> unit;

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


  (* Exceptions *)
  (* ************************************************************************ *)

  (* Internal exception *)
  exception Found of Ast.t * res

  (* Exception for typing errors *)
  exception Typing_error of error


  (* Warnings/Error helpers *)
  (* ************************************************************************ *)

  let _warn env fragment w =
    env.warnings (Warning (env, fragment, w))

  let _error env fragment e =
    raise (Typing_error (Error (env, fragment, e)))

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

  let _ty_var_app env v t =
    _error env (Ast t) (Ty_var_application v)

  let _var_app env v t =
    _error env (Ast t) (Var_application v)

  let _type_mismatch env t ty ast =
    _error env (Ast ast) (Type_mismatch (t, ty))

  let _record_type_mismatch env f ty_c ast =
    _error env (Ast ast) (Mismatch_record_type (f, ty_c))

  let _field_repeated env f ast =
    _error env (Ast ast) (Repeated_record_field f)

  let _field_missing env f ast =
    _error env (Ast ast) (Missing_record_field f)

  let _cannot_infer_quant_var env t =
    _error env (Ast t) (Quantified_var_inference)

  let _unknown_builtin env ast b =
    _error env (Ast ast) (Unhandled_builtin b)

  let _uncaught_exn env ast exn bt =
    _error env (Ast ast) (Uncaught_exn (exn, bt))

  let _cannot_find env ast s =
    _error env (Ast ast) (Cannot_find s)

  let _wrap env ast f arg =
    try f arg
    with
    | T.Wrong_type (t, ty) ->
      _type_mismatch env t ty ast
    | T.Wrong_record_type (f, c) ->
      _record_type_mismatch env f c ast
    | T.Field_repeated f ->
      _field_repeated env f ast
    | T.Field_missing f ->
      _field_missing env f ast
    | (Typing_error _) as exn ->
      raise exn
    | exn ->
      let bt = Printexc.get_raw_backtrace () in
      _uncaught_exn env ast exn bt

  let _wrap2 env ast f a b =
    _wrap env ast (fun () -> f a b) ()

  let _wrap3 env ast f a b c =
    _wrap env ast (fun () -> f a b c) ()

  (* Binding lookups *)
  (* ************************************************************************ *)

  let find_reason env (v : bound) =
    try
      let r =
        match v with
        | `Builtin _ -> Builtin
        | `Ty_var v -> E.find v env.type_locs
        | `Term_var v -> F.find v env.term_locs
        | `Letin (_, v, _) -> F.find v env.term_locs
        | `Ty_cst c -> R.find c env.st.ttype_locs
        | `Term_cst c -> S.find c env.st.const_locs
        | `Cstr c -> U.find c env.st.cstrs_locs
        | `Field f -> V.find f env.st.field_locs
      in
      Some r
    with Not_found -> assert false

  let with_reason reason bound : binding =
    match (bound : [ bound | not_found ]) with
    | `Not_found -> `Not_found
    | `Builtin `Ttype _ -> `Builtin `Ttype
    | `Builtin `Ty _ -> `Builtin `Ty
    | `Builtin `Term _ -> `Builtin `Term
    | `Builtin `Tags _ -> `Builtin `Tag
    | `Ty_var v -> `Variable (`Ty (v, reason))
    | `Term_var v -> `Variable (`Term (v, reason))
    | `Letin (_, v, _) -> `Variable (`Term (v, reason))
    | `Ty_cst c -> `Constant (`Ty (c, reason))
    | `Term_cst c -> `Constant (`Term (c, reason))
    | `Cstr c -> `Constant (`Cstr (c, reason))
    | `Field f -> `Constant (`Field (f, reason))

  let binding_reason binding : reason option =
    match (binding : binding) with
    | `Not_found -> assert false
    | `Builtin _ -> Some Builtin
    | `Variable `Ty (_, reason)
    | `Variable `Term (_, reason)
    | `Constant `Ty (_, reason)
    | `Constant `Term (_, reason)
    | `Constant `Cstr (_, reason)
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
    match M.find name env.vars with
    | #var as res -> res
    | exception Not_found -> `Not_found

  let find_global env id : [cst | not_found] =
    try (M.find id env.st.csts :> [cst | not_found])
    with Not_found -> `Not_found

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


  (* Global Environment *)
  (* ************************************************************************ *)

  let new_state () = {
    csts = M.empty;
    ttype_locs = R.empty;
    const_locs = S.empty;
    cstrs_locs = U.empty;
    field_locs = V.empty;
    custom = Hmap.empty;
  }

  let copy_state st = {
    csts = st.csts;
    custom = st.custom;
    ttype_locs = st.ttype_locs;
    const_locs = st.const_locs;
    cstrs_locs = st.cstrs_locs;
    field_locs = st.field_locs;
  }


  let add_global env fragment id reason (v : cst) =
    begin match find_bound env id with
      | `Not_found -> ()
      | #bound as old -> _shadow env fragment id old reason v
    end;
    env.st.csts <- M.add id v env.st.csts

  (* Symbol declarations *)
  let decl_ty_const env fg id c reason =
    add_global env fg id reason (`Ty_cst c);
    env.st.ttype_locs <- R.add c reason env.st.ttype_locs

  let decl_term_const env fg id c reason =
    add_global env fg id reason (`Term_cst c);
    env.st.const_locs <- S.add c reason env.st.const_locs

  let decl_term_cstr env fg id c reason =
    add_global env fg id reason (`Cstr c);
    env.st.cstrs_locs <- U.add c reason env.st.cstrs_locs

  let decl_term_field env fg id f reason =
    add_global env fg id reason (`Field f);
    env.st.field_locs <- V.add f reason env.st.field_locs


  (* Custom theory data in the global state *)
  let get_global_custom env key =
    match Hmap.get env.st.custom key with
    | x :: _ -> Some x
    | [] -> None

  let set_global_custom env key value =
    env.st.custom <- Hmap.replace env.st.custom key [value]


  (* Local Environment *)
  (* ************************************************************************ *)

  let global = new_state ()

  (* Make a new empty environment *)
  let empty_env
      ?(st=global)
      ?(expect=Nothing)
      ?(infer_hook=(fun _ _ -> ()))
      ?infer_base
      ?(poly=Flexible)
      ?(quants=true)
      ~warnings ~file
      builtins = {
    file; st; builtins; warnings;
    poly; quants; expect; infer_hook; infer_base;
    vars = M.empty;
    type_locs = E.empty;
    term_locs = F.empty;
  }

  let expect ?(force=false) env expect =
    if env.expect = Nothing && not force then env
    else { env with expect = expect }

  (* Generate new fresh names for shadowed variables *)
  let new_name pre =
    let i = ref 0 in
    (fun () -> incr i; pre ^ (string_of_int !i))

  let new_ty_name = new_name "ty#"
  let new_term_name = new_name "term#"

  (* Add local variables to environment *)
  let add_type_var env id v ast =
    let reason = Bound (env.file, ast) in
    let v' =
      match find_bound env id with
      | `Not_found -> v
      | #bound as old ->
        let v' = Ty.Var.mk (new_ty_name ()) in
        _shadow env (Ast ast) id old reason (`Ty_var v');
        v'
    in
    v', { env with
          vars = M.add id (`Ty_var v') env.vars;
          type_locs = E.add v' reason env.type_locs;
        }

  let add_type_vars env l =
    let l', env' = List.fold_left (fun (l, acc) (id, v, ast) ->
        let v', acc' = add_type_var acc id v ast in
        v' :: l, acc') ([], env) l in
    List.rev l', env'

  let add_term_var env id v ast =
    let reason = Bound (env.file, ast) in
    let v' =
      match find_bound env id with
      | `Not_found -> v
      | #bound as old ->
        let v' = T.Var.mk (new_term_name ()) (T.Var.ty v) in
        _shadow env (Ast ast) id old reason (`Term_var v');
        v'
    in
    v', { env with
          vars = M.add id (`Term_var v') env.vars;
          term_locs = F.add v' reason env.term_locs;
        }

  let bind_term_var env id e v t ast =
    let reason = Bound (env.file, ast) in
    let v' =
      match find_bound env id with
      | `Not_found -> v
      | #bound as old ->
        let v' = T.Var.mk (new_term_name ()) (T.Var.ty v) in
        _shadow env (Ast ast) id old reason (`Term_var v');
        v'
    in
    let t' = T.bind v' t in
    v', { env with
          vars = M.add id (`Letin (e, v', t')) env.vars;
          term_locs = F.add v' reason env.term_locs;
        }


  (* Typo suggestion *)
  (* ************************************************************************ *)

  let suggest ~limit env id =
    let automaton = Spelll.of_string ~limit Id.(id.name) in
    let aux id _ acc =
      if Spelll.match_with automaton Id.(id.name)
      then id :: acc
      else acc
    in
    M.fold aux env.st.csts (M.fold aux env.vars [])


  (* Typing explanation *)
  (* ************************************************************************ *)

  let _unused_type env v =
    match E.find v env.type_locs with
    (* Variable bound or inferred *)
    | Bound (_, t) | Inferred (_, t) ->
      _warn env (Ast t) (Unused_type_variable v)
    (* variables should not be declare-able nor builtin *)
    | Builtin | Declared _ | Defined _ ->
      assert false

  let _unused_term env v =
    match F.find v env.term_locs with
    (* Variable bound or inferred *)
    | Bound (_, t) | Inferred (_, t) ->
      _warn env (Ast t) (Unused_term_variable v)
    (* variables should not be declare-able nor builtin *)
    | Builtin | Declared _ | Defined _ ->
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

  (* Split arguments of a function/constructor application *)
  let split_args env n_ty n_t args =
    let n_args = List.length args in
    match env.poly with
    | Explicit ->
      if n_args = n_ty + n_t then
        `Ok (Misc.Lists.take_drop n_ty args)
      else
        `Bad_arity ([n_ty + n_t], n_args)
    | Implicit ->
      if n_args = n_t then
        `Ok (Misc.Lists.init n_ty (fun _ -> Ast.wildcard ()), args)
      else
        `Bad_arity ([n_t], n_args)
    | Flexible ->
      if n_args = n_ty + n_t then
        `Ok (Misc.Lists.take_drop n_ty args)
      else if n_args = n_t then
        `Ok (Misc.Lists.init n_ty (fun _ -> Ast.wildcard ()), args)
      else
        `Bad_arity ([n_t; n_ty + n_t], n_args)


  (* wrapper for builtin application *)
  let builtin_apply env b ast args : res =
    match (b : builtin_res) with
    | `Ttype f -> _wrap2 env ast f ast args; Ttype
    | `Ty f -> Ty (_wrap2 env ast f ast args)
    | `Term f -> Term (_wrap2 env ast f ast args)
    | `Tags f -> Tags (_wrap2 env ast f ast args)

  (* Wrapper around record creation *)
  let create_record env ast l =
    _wrap env ast T.record l

  let create_record_with env ast t l =
    _wrap2 env ast T.record_with t l

  let create_record_access env ast t field =
    _wrap2 env ast T.apply_field field t

  let make_eq env ast_term a b =
    _wrap2 env ast_term T.eq a b

  let ty_var_equal v v' = Ty.Var.compare v v' = 0
  let t_var_equal v v' = T.Var.compare v v' = 0

  let mk_quant env ast mk (ty_vars, t_vars) body =
    if not env.quants then
      _error env (Ast ast) Forbidden_quantifier
    else begin
      let fv_ty, fv_t = T.fv body in
      (* Emit warnings for quantified variables that are unused *)
      List.iter (fun v ->
          if not @@ List.exists (ty_var_equal v) fv_ty then _unused_type env v
        ) ty_vars;
      List.iter (fun v ->
          if not @@ List.exists (t_var_equal v) fv_t then _unused_term env v
        ) t_vars;
      (* Filter quantified variables from free_variables *)
      let fv_ty = List.filter (fun v ->
          not (List.exists (ty_var_equal v) ty_vars)) fv_ty in
      let fv_t = List.filter (fun v ->
          not (List.exists (t_var_equal v) t_vars)) fv_t in
      (* Create the quantified formula *)
      _wrap3 env ast mk (fv_ty, fv_t) (ty_vars, t_vars) body
    end

  let infer env ast s args s_ast =
    if Id.(s.ns = Var) then
      _error env (Ast ast) Infer_type_variable;
    match env.expect, env.infer_base with
    | Nothing, _ -> None
    | Type, _ ->
      let n = List.length args in
      let ret = Ty.Const.mk (Id.full_name s) n in
      let res = Ty_fun ret in
      env.infer_hook env res;
      decl_ty_const env (Ast ast) s ret (Inferred (env.file, s_ast));
      Some res
    | Typed _, None -> None
    | Typed ty, Some base ->
      let n = List.length args in
      let ret = T.Const.mk
          (Id.full_name s) [] (Misc.Lists.replicate n base) ty
      in
      let res = Term_fun ret in
      env.infer_hook env res;
      decl_term_const env (Ast ast) s ret (Inferred (env.file, s_ast));
      Some res


  (* Tag application *)
  (* ************************************************************************ *)

  let apply_tag env ast tag v res =
    match (res : res) with
    | Ttype -> _error env (Ast ast) Cannot_tag_ttype
    | Tags _ -> _error env (Ast ast) Cannot_tag_tag
    | Ty ty -> Ty.tag ty tag v
    | Term t -> T.tag t tag v

  (* Expression parsing *)
  (* ************************************************************************ *)

  let expect_base env =
    match env.infer_base with
    | None -> env
    | Some ty -> expect env (Typed ty)

  let expect_prop env =
    expect env (Typed Ty.prop)

  let rec parse_expr (env : env) t : res =
    let res : res = match t with

      (* Ttype & builtin types *)
      | { Ast.term = Ast.Builtin Ast.Ttype; _ } ->
        Ttype
      | { Ast.term = Ast.Builtin Ast.Prop; _ } ->
        Ty Ty.prop

      (* Wildcards should only occur in place of types *)
      | { Ast.term = Ast.Builtin Ast.Wildcard; _ } ->
        Ty (Ty.wildcard ())

      (* Basic formulas *)
      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.True; _ }, []); _ }
      | { Ast.term = Ast.Builtin Ast.True; _ } ->
        Term T._true

      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.False; _ }, []); _ }
      | { Ast.term = Ast.Builtin Ast.False; _ } ->
        Term T._false

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.And; _ }, l); _ } ->
        Term (_wrap env t T._and (List.map (parse_prop env) l))

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Or; _ }, l); _ } ->
        Term (_wrap env t T._or (List.map (parse_prop env) l))

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Xor; _}, l); _ } as t ->
        begin match l with
          | [p; q] ->
            let f = parse_prop env p in
            let g = parse_prop env q in
            Term (_wrap2 env t T.xor f g)
          | _ -> _bad_op_arity env "xor" 2 (List.length l) t
        end

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Nand; _}, l); _ } as t ->
        begin match l with
          | [p; q] ->
            let f = parse_prop env p in
            let g = parse_prop env q in
            Term (_wrap2 env t T.nand f g)
          | _ -> _bad_op_arity env "nand" 2 (List.length l) t
        end

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Nor; _}, l); _ } as t ->
        begin match l with
          | [p; q] ->
            let f = parse_prop env p in
            let g = parse_prop env q in
            Term (_wrap2 env t T.nor f g)
          | _ -> _bad_op_arity env "nor" 2 (List.length l) t
        end

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Imply; _ }, l); _ } as t ->
        begin match l with
          | [p; q] ->
            let f = parse_prop env p in
            let g = parse_prop env q in
            Term (_wrap2 env t T.imply f g)
          | _ -> _bad_op_arity env "=>" 2 (List.length l) t
        end

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Implied; _ }, l); _ } as t ->
        begin match l with
          | [q; p] ->
            let f = parse_prop env p in
            let g = parse_prop env q in
            Term (_wrap2 env t T.imply f g)
          | _ -> _bad_op_arity env "<=" 2 (List.length l) t
        end

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv; _}, l); _ } as t ->
        begin match l with
          | [p; q] ->
            let f = parse_prop env p in
            let g = parse_prop env q in
            Term (_wrap2 env t T.equiv f g)
          | _ -> _bad_op_arity env "<=>" 2 (List.length l) t
        end

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Not; _}, l); _ } as t ->
        begin match l with
          | [p] ->
            Term (_wrap env t T.neg (parse_prop env p))
          | _ -> _bad_op_arity env "not" 1 (List.length l) t
        end

      (* Binders *)
      | { Ast.term = Ast.Binder (Ast.All, _, _); _ } ->
        parse_quant T.all Ast.All env t [] [] t

      | { Ast.term = Ast.Binder (Ast.Ex, _, _); _ } ->
        parse_quant T.ex Ast.Ex env t [] [] t

      (* Pattern matching *)
      | { Ast.term = Ast.Match (scrutinee, branches); _ } ->
        parse_match env t scrutinee branches

      (* (Dis)Equality *)
      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq; _ }, l); _ } as t ->
        begin match l with
          | [a; b] ->
            let env = expect_base env in
            begin match parse_expr env a, parse_expr env b with
            | Term t1, Term t2 -> Term (make_eq env t t1 t2)
            | _ -> _expected env "two terms" t None
            end
          | _ -> _bad_op_arity env "=" 2 (List.length l) t
        end

      | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Distinct; _}, args); _ } ->
        let l' = List.map (parse_term env) args in
        Term (_wrap env t T.distinct l')

      (* General case: application *)
      | { Ast.term = Ast.Symbol s; _ } as ast ->
        parse_app env ast s ast []
      | { Ast.term = Ast.App (
          { Ast.term = Ast.Symbol s; _ } as s_ast, l); _ } as ast ->
        parse_app env ast s s_ast l

      (* If-then-else *)
      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Ite; _}, l); _ } as ast ->
        parse_ite env ast l

      (* Record creation *)
      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Record; _ }, l); _ } as ast ->
        parse_record env ast l

      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Record_with; _ }, l); _ } as ast ->
        parse_record_with env ast l

      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Record_access; _ }, l); _ } as ast ->
        parse_record_access env ast l

      (* Builtin application not treated directly, but instead
         routed to a semantic extension through builtin_symbols. *)
      | { Ast.term = Ast.Builtin b; _ } as ast ->
        parse_builtin env ast b []
      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin b; _ }, l); _ } as ast ->
        parse_builtin env ast b l

      (* Local bindings *)
      | { Ast.term = Ast.Binder (Ast.Let, vars, f); _ } ->
        parse_let env [] f vars

      (* Type annotations *)
      | { Ast.term = Ast.Colon (a, expected); _ } ->
        parse_ensure env a expected

      (* Sometimes parser creates extra applications *)
      | { Ast.term = Ast.App (t, []); _ } ->
        parse_expr env t

      (* Explicitly catch higher-order application. *)
      | { Ast.term = Ast.App ({ Ast.term = Ast.App _; _ }, _); _ } as ast ->
        _error env (Ast ast) Higher_order_application

      (* Other cases *)
      | ast -> _error env (Ast ast) Unhandled_ast
    in
    apply_attr env res t t.Ast.attr

  and apply_attr env res ast l =
    let () = List.iter (function
        | Any (tag, v) ->
          apply_tag env ast tag v res;
      ) (parse_attrs env ast [] l) in
    res

  and parse_attr_and env ast =
    match ast with
    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.And; _ }, l); _ } ->
      parse_attrs env ast [] l
    | _ -> parse_attrs env ast [] [ast]

  and parse_attrs env ast acc = function
    | [] -> acc
    | a :: r ->
      begin match parse_expr (expect env Nothing) a with
        | Tags l ->
          parse_attrs env ast (l @ acc) r
        | res ->
          _expected env "tag" a (Some res)
        | exception (Typing_error Error (_, Ast t, _) as exn) ->
          _warn env (Ast t) (Error_in_attribute exn);
          parse_attrs env ast acc r
        | exception exn ->
          _warn env (Ast a) (Error_in_attribute exn);
          parse_attrs env ast acc r
      end

  and parse_var env = function
    | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s; _ }, e); _ } ->
      begin match parse_expr env e with
        | Ttype -> `Ty (s, Ty.Var.mk (Id.full_name s))
        | Ty ty -> `Term (s, T.Var.mk (Id.full_name s) ty)
        | res -> _expected env "type (or Ttype)" e (Some res)
      end
    | { Ast.term = Ast.Symbol s; _ } as t ->
      begin match env.expect with
        | Nothing -> _cannot_infer_quant_var env t
        | Type -> `Ty (s, Ty.Var.mk (Id.full_name s))
        | Typed ty -> `Term (s, T.Var.mk (Id.full_name s) ty)
      end
    | t -> _expected env "(typed) variable" t None

  and parse_quant_vars env l =
    let ttype_vars, typed_vars, env' = List.fold_left (
        fun (l1, l2, acc) v ->
          match parse_var acc v with
          | `Ty (id, v') ->
            let v'', acc' = add_type_var acc id v' v in
            (v'' :: l1, l2, acc')
          | `Term (id, v') ->
            let v'', acc' = add_term_var acc id v' v in
            (l1, v'' :: l2, acc')
      ) ([], [], env) l in
    List.rev ttype_vars, List.rev typed_vars, env'

  and parse_quant mk b env ast ttype_acc ty_acc = function
    | { Ast.term = Ast.Binder (b', vars, f); _ } when b = b' ->
      let ttype_vars, ty_vars, env' = parse_quant_vars (expect_base env) vars in
      parse_quant mk b env' ast (ttype_acc @ ttype_vars) (ty_acc @ ty_vars) f
    | body_ast ->
      let body = parse_prop env body_ast in
      let f = mk_quant env ast mk (ttype_acc, ty_acc) body in
      Term f

  and parse_match env ast scrutinee branches =
    let t = parse_term env scrutinee in
    let l = List.map (parse_branch (T.ty t) env) branches in
    Term (_wrap2 env ast T.pattern_match t l)

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
    let v = T.Var.mk (Id.full_name s) ty in
    let v, env = add_term_var env s v ast in
    T.of_var v, env

  and parse_pattern_app_cstr ty env t c args =
    (* Inlined version of parse_app_cstr *)
    let n_ty, n_t = T.Cstr.arity c in
    let ty_l, t_l =
      match split_args env n_ty n_t args with
      | `Ok (l, l') -> l, l'
      | `Bad_arity (expected, actual) ->
        _bad_cstr_arity env c expected actual t
    in
    (* We can't allow binding new type variables here *)
    let ty_args = List.map (parse_ty env) ty_l in
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

  and parse_let env acc f = function
    | [] -> (* TODO: use continuation to avoid stack overflow on packs of let-bindings ? *)
      let l = List.rev acc in
      begin match parse_expr env f with
        | Term t -> Term (T.letin l t)
        | res -> _expected env "term of formula" f (Some res)
      end
    | x :: r ->
      begin match x with
        | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s; _ } as w, e); _ }
        | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq; _}, [
                { Ast.term = Ast.Symbol s; _ } as w; e]); _ }
        | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv; _}, [
                { Ast.term = Ast.Symbol s; _ } as w; e]); _ } ->
          let t = parse_term env e in
          let v = T.Var.mk (Id.full_name s) (T.ty t) in
          let v', env' = bind_term_var env s e v t w in
          parse_let env' ((v', t) :: acc) f r
        | t -> _expected env "variable binding" t None
      end

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
      _bad_op_arity env "field record access" 2 (List.length l) ast

  and parse_app env ast s s_ast args =
    match find_bound env s with
    | `Ty_var v ->
      if args = [] then Ty (Ty.of_var v)
      else _ty_var_app env v ast
    | `Term_var v ->
      if args = [] then Term (T.of_var v)
      else _var_app env v ast
    | `Letin (_, v, t) ->
      if args = [] then Term t
      else _var_app env v ast
    | `Ty_cst f ->
      parse_app_ty env ast f args
    | `Term_cst f ->
      parse_app_term env ast f args
    | `Cstr c ->
      parse_app_cstr env ast c args
    | `Field _f ->
      _expected env "not a field name" s_ast None
    | `Builtin b ->
      builtin_apply env b ast args
    | `Not_found ->
      begin match infer env ast s args s_ast with
        | Some Ty_fun f -> parse_app_ty env ast f args
        | Some Term_fun f -> parse_app_term env ast f args
        | None -> _cannot_find env ast s
      end

  and parse_app_ty env ast f args =
    if List.length args <> Ty.Const.arity f then
      _bad_ty_arity env f (List.length args) ast;
    let l = List.map (parse_ty env) args in
    Ty (Ty.apply f l)

  and parse_app_term env ast f args =
    let n_ty, n_t = T.Const.arity f in
    let ty_l, t_l =
      match split_args env n_ty n_t args with
      | `Ok (l, l') -> l, l'
      | `Bad_arity (expected, actual) ->
        _bad_term_arity env f expected actual ast
    in
    let ty_args = List.map (parse_ty env) ty_l in
    let t_args = List.map (parse_term env) t_l in
    Term (_wrap3 env ast T.apply f ty_args t_args)

  and parse_app_cstr env ast c args =
    let n_ty, n_t = T.Cstr.arity c in
    let ty_l, t_l =
      match split_args env n_ty n_t args with
      | `Ok (l, l') -> l, l'
      | `Bad_arity (expected, actual) ->
        _bad_cstr_arity env c expected actual ast
    in
    let ty_args = List.map (parse_ty env) ty_l in
    let t_args = List.map (parse_term env) t_l in
    Term (_wrap3 env ast T.apply_cstr c ty_args t_args)

  and parse_ite env ast = function
    | [c; a; b] ->
      let cond = parse_prop env c in
      let then_t = parse_term env a in
      let else_t = parse_term env b in
      Term (_wrap3 env ast T.ite cond then_t else_t)
    | args ->
      _bad_op_arity env "#ite" 3 (List.length args) ast

  and parse_ensure env a expected =
    let t = parse_term env a in
    let ty = parse_ty env expected in
    Term (T.ensure t ty)

  and parse_builtin env ast b args =
    match env.builtins env (Builtin b) with
    | `Not_found -> _unknown_builtin env ast b
    | #builtin_res as b -> builtin_apply env b ast args

  and parse_ty env ast =
    unwrap_ty env ast (parse_expr (expect env Type) ast)

  and parse_term env ast =
    unwrap_term env ast (parse_expr (expect_base env) ast)

  and parse_prop env ast =
    match parse_expr (expect_prop env) ast with
    | Term t -> t
    | res -> _expected env "term/prop" ast (Some res)

  let parse_ttype_var env t =
    match parse_var (expect ~force:true env Type) t with
    | `Ty (id, v) -> (id, v, t)
    | `Term (_, v) ->
      _expected env "type variable" t (Some (Term (T.of_var v)))

  let rec parse_sig_quant env = function
    | { Ast.term = Ast.Binder (Ast.Pi, vars, t); _ } ->
      let ttype_vars = List.map (parse_ttype_var env) vars in
      let ttype_vars', env' = add_type_vars env ttype_vars in
      let l = List.combine vars ttype_vars' in
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
          begin match ttype_vars with
            | (h, _) :: _ ->
              _error env (Ast h) Type_var_in_type_constructor
            | [] ->
              let aux n arg =
                match (arg : _ * res) with
                | (_, Ttype) -> n + 1
                | (ast, res) -> raise (Found (ast, res))
              in
              begin
                match List.fold_left aux 0 ty_args with
                | n -> `Ty_cstr n
                | exception Found (err, _) ->
                  _error env (Ast err) Type_var_in_type_constructor
              end
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
    | { Ast.term = Ast.Binder (Ast.Arrow, _, _); _ } as ast ->
      _error env (Ast ast) Higher_order_type
    | t ->
      [t, parse_expr env t]

  let parse_sig = parse_sig_quant

  let rec parse_fun ty_args t_args env = function
    | { Ast.term = Ast.Binder (Ast.Fun, l, ret); _ } ->
      let ty_args', t_args', env' = parse_quant_vars env l in
      parse_fun (ty_args @ ty_args') (t_args @ t_args') env' ret
    | ast ->
      begin match parse_expr env ast with
        | (Ty body) as res ->
          if t_args = [] then `Ty (ty_args, body)
          else _expected env "non_dependant type (or a term)" ast (Some res)
        | Term body -> `Term (ty_args, t_args, body)
        | res -> _expected env "term or a type" ast (Some res)
      end

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
    let ttype_vars = List.map (parse_ttype_var env) vars in
    let ty_vars, env = add_type_vars env ttype_vars in
    let l = List.map (fun (id, t) ->
        let ty = parse_ty env t in
        Id.full_name id, ty
      ) fields in
    let field_list = T.define_record ty_cst ty_vars l in
    List.iter2 (fun (id, _) field ->
        decl_term_field env (Decl d) id field (Declared (env.file, d))
      ) fields field_list

  let inductive env d ty_cst { Stmt.id; vars; cstrs; _ } =
    (* Parse the type variables *)
    let ttype_vars = List.map (parse_ttype_var env) vars in
    let ty_vars, env = add_type_vars env ttype_vars in
    (* Parse the constructors *)
    let cstrs_with_ids = List.map (fun (id, args) ->
        id, List.map (fun t ->
            let ty, dstr = parse_inductive_arg env t in
            t, ty, dstr
          ) args
      ) cstrs in
    (* Constructors with strings for names *)
    let cstrs_with_strings = List.map (fun (id, args) ->
        Id.full_name id, List.map (fun (_, ty, dstr) ->
            ty, Misc.Options.map Id.full_name dstr
          ) args
      ) cstrs_with_ids in
    (* Call the T module to define the adt and get the typed constructors
       and destructors. *)
    let defined_cstrs = T.define_adt ty_cst ty_vars cstrs_with_strings in
    (* Register the constructors and destructors in the global env. *)
    List.iter2 (fun (cid, pargs) (c, targs) ->
        decl_term_cstr env (Decl d) cid c (Declared (env.file, d));
        List.iter2 (fun (t, _, dstr) (_, o) ->
            match dstr, o with
            | None, None -> ()
            | None, Some c ->
              _warn env (Ast t) (Superfluous_destructor (id, cid, c))
            | Some id, Some const ->
              decl_term_const env (Decl d) id const (Declared (env.file, d))
            | Some id, None ->
              _error env (Ast t) (Missing_destructor id)
          ) pargs targs
      ) cstrs_with_ids defined_cstrs

  let define_decl env (_, cst) t =
    match cst, (t : Stmt.decl) with
    | _, Abstract _ -> ()
    | `Term_decl _, Inductive _ -> assert false
    | `Type_decl c, Inductive i -> inductive env t c i
    | `Term_decl _, Record _ -> assert false
    | `Type_decl c, Record r -> record env t c r

  let parse_decl env tags (t : Stmt.decl) =
    match t with
    | Abstract { id; ty; _ } ->
      begin match parse_sig env ty with
        | `Ty_cstr n ->
          let c = Ty.Const.mk (Id.full_name id) n in
          List.iter (fun (Any (tag, v)) -> Ty.Const.tag c tag v) tags;
          id, `Type_decl c
        | `Fun_ty (vars, args, ret) ->
          let f = T.Const.mk (Id.full_name id) vars args ret in
          List.iter (fun (Any (tag, v)) -> T.Const.tag f tag v) tags;
          id, `Term_decl f
      end
    | Record { id; vars; _ }
    | Inductive { id; vars; _ } ->
      let n = List.length vars in
      let c = Ty.Const.mk (Id.full_name id) n in
      List.iter (fun (Any (tag, v)) -> Ty.Const.tag c tag v) tags;
      id, `Type_decl c

  let record_decl env (id, tdecl) (t : Stmt.decl) =
    match tdecl with
    | `Type_decl c -> decl_ty_const env (Decl t) id c (Declared (env.file, t))
    | `Term_decl f -> decl_term_const env (Decl t) id f (Declared (env.file, t))

  let decls env ?attr (d: Stmt.decl Stmt.group) =
    let tags = match attr with
      | None -> []
      | Some a -> parse_attr_and env a
    in
    if d.recursive then begin
      (* Check well-foundedness *)
      check_well_founded env d d.contents;
      (* First pre-parse all definitions and generate the typed symbols for them *)
      let parsed = List.map (parse_decl env tags) d.contents in
      (* Then, since the decls are recursive, register in the global env the type
         const for each decl before fully parsing and defining them. *)
      let () = List.iter2 (record_decl env) parsed d.contents in
      (* Then parse the complete type definition and define them.
         TODO: parse (and thus define them with T) in the topological order
             defined by the well-founded check ? *)
      List.iter2 (define_decl env) parsed d.contents;
      (* Return the defined types *)
      List.map snd parsed
    end else begin
      List.map (fun t ->
          (* First pre-parse all definitions and generate the typed symbols for them *)
          let parsed = parse_decl env tags t in
          (* Then parse the complete type definition and define them. *)
          let () = define_decl env parsed t in
          (* Finally record them in the state *)
          let () = record_decl env parsed t in
          (* return *)
          snd parsed
        ) d.contents
    end

  (* Definitions *)
  (* ************************************************************************ *)

  let parse_def_sig env tags (d: Stmt.def) =
    parse_decl env tags (Abstract { id = d.id; ty = d.ty; loc = d.loc; })

  let record_def d env (id, tdecl) (t : Stmt.def) =
    match tdecl with
    | `Type_decl _ -> _error env (Defs d) (Type_def_rec t)
    | `Term_decl f -> decl_term_const env (Def t) id f (Defined (env.file, t))

  let parse_def env (_, ssig) (d : Stmt.def) =
    match ssig, parse_fun [] [] env d.body with
    | `Type_decl c, `Ty (ty_args, body) ->
      `Type_def (d.id, c, ty_args, body)
    | `Term_decl f, `Term (ty_args, t_args, body) ->
      `Term_def (d.id, f, ty_args, t_args, body)
    | `Term_decl _, `Ty _ -> assert false
    | `Type_decl _, `Term _ -> assert false

  let defs env ?attr (d : Stmt.defs) =
    let tags = match attr with
      | None -> []
      | Some a -> parse_attr_and env a
    in
    if d.recursive then begin
      let sigs = List.map (parse_def_sig env tags) d.contents in
      let () = List.iter2 (record_def d env) sigs d.contents in
      List.map2 (parse_def env) sigs d.contents
    end else begin
      List.map (fun t ->
          parse_def env (parse_def_sig env tags t) t
        ) d.contents
    end

  (* High-level parsing function *)
  (* ************************************************************************ *)

  let parse env ast =
    let res = parse_prop env ast in
    match T.fv res with
    | [], [] -> res
    | tys, ts -> _error env (Ast ast) (Unbound_variables (tys, ts, res))

end
