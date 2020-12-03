
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Module aliases *)
(* ************************************************************************ *)

(* Convenient aliases *)
module Tag = Dolmen.Std.Tag
module Expr = Dolmen.Std.Expr
module Ty = Dolmen.Std.Expr.Ty
module Term = Dolmen.Std.Expr.Term
module Var = Dolmen.Std.Expr.Var
module Cst = Dolmen.Std.Expr.Cst
module Cstr = Dolmen.Std.Expr.Cstr
module Field = Dolmen.Std.Expr.Field
module View = Dolmen.Std.Expr.View

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

(* Different behavior of polymorphism wrt application *)
type poly =
  | Explicit
  | Implicit
  | Flexible

(* Allowed order of terms/types *)
type order =
  | First_order
  | Higher_order
  | Signature of order

(* The various schemes of quantification allowed *)
type quant =
  | No_quantifiers
  | Only_terms
  | Only_types
  | Prenex_types
  | Dependant

(* The patterns allowed: shallow or deep. *)
type pattern =
  | Deep
  | Shallow

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
  | Expr  of Expr.t
  | Tags  of tag list

(* Things that can be inferred *)
type inferred = Fun of Cst.t

(* Wrapper around potential function symbols in Dolmen *)
type symbol =
  | Id of Id.t
  | Builtin of Ast.builtin

(* Not found result *)
type not_found = [ `Not_found ]

(* Variable that can be bound to a dolmen identifier *)
type var = [
  | `Var of Var.t
  | `Letin of Ast.t * Var.t * Expr.t
]

(* Constants that can be bound to a dolmen identifier. *)
type cst = [
  | `Cst of Cst.t
  | `Cstr of Cstr.t
  | `Field of Field.t
]

(* Result of parsing a symbol by the theory *)
type builtin_res = [
  | `Expr  of (Ast.t -> Ast.t list -> Expr.t)
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
      | `Expr
      | `Tag
    ]
  | `Variable of Var.t * reason option
  | `Constant of [
      | `Cst of Cst.t * reason option
      | `Cstr of Cstr.t * reason option
      | `Field of Field.t * reason option
    ]
]
(** The bindings that can occur. *)

(* Maps & Hashtbls *)
(* ************************************************************************ *)

module M = Map.Make(Id)

(*
module E = Map.Make(Var)
module F = Map.Make(Var)
module R = Map.Make(Cst)
module S = Map.Make(Cst)
module U = Map.Make(Cstr)
module V = Map.Make(Field)
*)

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
  | Unused_variable : Var.t -> Ast.t warn
  (* Unused quantified type variable *)
  | Error_in_attribute : exn -> Ast.t warn
  (* An error occurred wile parsing an attribute *)
  | Superfluous_destructor : Id.t * Id.t * Cst.t -> Ast.t warn
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
  | Bad_op_arity : string * int list * int -> Ast.t err
  | Bad_cst_arity : Cst.t * int list * int -> Ast.t err
  | Repeated_record_field : Field.t -> Ast.t err
  | Missing_record_field : Field.t -> Ast.t err
  | Mismatch_record_type : Field.t * Cst.t -> Ast.t err
  | Var_application : Var.t -> Ast.t err
  | Type_mismatch : Expr.t * Ty.t -> Ast.t err
  | Quantified_var_inference : Ast.t err
  | Unhandled_builtin : Ast.builtin -> Ast.t err
  | Cannot_tag_tag : Ast.t err
  | Cannot_tag_ttype : Ast.t err
  | Cannot_find : Id.t -> Ast.t err
  | Type_var_in_type_constructor : Var.t -> Ast.t err
  | Forbidden_quantifier : Ast.t err
  | Missing_destructor : Id.t -> Ast.t err
  | Type_def_rec : Stmt.def -> Stmt.defs err
  | Higher_order_application : Ast.t err
  | Higher_order_type : Ast.t err
  | Unbound_variables : Var.t list * Var.t list * Expr.t -> Ast.t err
  | Uncaught_exn : exn * Printexc.raw_backtrace -> Ast.t err
  | Unhandled_ast : Ast.t err


(* State & Environment *)
(* ************************************************************************ *)

(* Global, mutable state. *)
type state = {

  mutable csts : cst M.t;
  (* association between dolmen ids and types/terms constants. *)

  mutable const_locs : reason Cst.Map.t;
  (* stores reasons for typing of constants *)
  mutable cstrs_locs : reason Cstr.Map.t;
  (* stores reasons for typing constructors *)
  mutable field_locs : reason Field.Map.t;
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

  (* bound variables *)
  vars : var M.t;

  (* Map from term variables to the reason of its type *)
  vars_loc : reason Var.Map.t;

  (* The current builtin symbols *)
  builtins : builtin_symbols;

  (* warnings *)
  warnings : warning -> unit;

  (* Additional typing info *)
  order        : order;   (* first order, higher order, etc.. *)
  poly_app     : poly;    (* kind of polymorphism allowed *)
  poly_pat     : poly;    (* polymorphism allowed in patterns *)
  quants       : quant;   (* what quantifications are allowed *)
  pattern      : pattern; (* patterns allowed in pattern matches *)

  expect       : expect;
  infer_base   : Ty.t option; (* base type for inferred constants (if any) *)
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

let _bad_cst_arity env f expected actual t =
  _error env (Ast t) (Bad_cst_arity (f, expected, actual))

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
  | Dolmen.Std.Expr.Wrong_type { term = t; expected_ty = ty; } ->
    _type_mismatch env t ty ast
  | Dolmen.Std.Expr.Wrong_record_type { cst = f; record_ty = c; } ->
    _record_type_mismatch env f c ast
  | Dolmen.Std.Expr.Field_repeated f ->
    _field_repeated env f ast
  | Dolmen.Std.Expr.Field_missing f ->
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
      | `Var v -> Var.Map.find v env.vars_loc
      | `Letin (_, v, _) -> Var.Map.find v env.vars_loc
      | `Cst c -> Cst.Map.find c env.st.const_locs
      | `Cstr c -> Cstr.Map.find c env.st.cstrs_locs
      | `Field f -> Field.Map.find f env.st.field_locs
    in
    Some r
  with Not_found -> assert false

let with_reason reason bound : binding =
  match (bound : [ bound | not_found ]) with
  | `Not_found -> `Not_found
  | `Builtin `Expr _ -> `Builtin `Expr
  | `Builtin `Tags _ -> `Builtin `Tag
  | `Var v -> `Variable (v, reason)
  | `Letin (_, v, _) -> `Variable (v, reason)
  | `Cst c -> `Constant (`Cst (c, reason))
  | `Cstr c -> `Constant (`Cstr (c, reason))
  | `Field f -> `Constant (`Field (f, reason))

let binding_reason binding : reason option =
  match (binding : binding) with
  | `Not_found -> assert false
  | `Builtin _ -> Some Builtin
  | `Variable (_, reason)
  | `Constant `Cst (_, reason)
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
  const_locs = Cst.Map.empty;
  cstrs_locs = Cstr.Map.empty;
  field_locs = Field.Map.empty;
  custom = Hmap.empty;
}

let copy_state st = {
  csts = st.csts;
  custom = st.custom;
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
let decl_const env fg id c reason =
  add_global env fg id reason (`Cst c);
  env.st.const_locs <- Cst.Map.add c reason env.st.const_locs

let decl_cstr env fg id c reason =
  add_global env fg id reason (`Cstr c);
  env.st.cstrs_locs <- Cstr.Map.add c reason env.st.cstrs_locs

let decl_field env fg id f reason =
  add_global env fg id reason (`Field f);
  env.st.field_locs <- Field.Map.add f reason env.st.field_locs


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
    ?(order=Higher_order)
    ?(poly_app=Flexible)
    ?(poly_pat=Implicit)
    ?(quants=Prenex_types)
    ?(pattern=Deep)
    ~warnings ~file
    builtins = {
  file; st;
  builtins; warnings;
  order; poly_app; poly_pat; quants; pattern;
  expect; infer_base; infer_hook;
  vars = M.empty; vars_loc = Var.Map.empty;
}

let expect ?(force=false) env expect =
  if env.expect = Nothing && not force then env
  else { env with expect; }

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
      let v' = Ty.var (new_ty_name ()) in
      _shadow env (Ast ast) id old reason (`Var v');
      v'
  in
  v', { env with
        vars = M.add id (`Var v') env.vars;
        vars_loc = Var.Map.add v' reason env.vars_loc;
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
      let v' = Var.mk (new_term_name ()) (Var.ty v) in
      _shadow env (Ast ast) id old reason (`Var v');
      v'
  in
  v', { env with
        vars = M.add id (`Var v') env.vars;
        vars_loc = Var.Map.add v' reason env.vars_loc;
      }

let bind_term_var env id e v t ast =
  let reason = Bound (env.file, ast) in
  let v' =
    match find_bound env id with
    | `Not_found -> v
    | #bound as old ->
      let v' = Var.mk (new_term_name ()) (Var.ty v) in
      _shadow env (Ast ast) id old reason (`Var v');
      v'
  in
  v', { env with
        vars = M.add id (`Letin (e, v', t)) env.vars;
        vars_loc = Var.Map.add v' reason env.vars_loc;
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

let _unused_var env v =
  match Var.Map.find v env.vars_loc with
  (* Variable bound or inferred *)
  | Bound (_, t) | Inferred (_, t) ->
    _warn env (Ast t) (Unused_variable v)
  (* variables should not be declare-able nor builtin *)
  | Builtin | Declared _ | Defined _ ->
    assert false


(* Wrappers for expression building *)
(* ************************************************************************ *)

(* Expected number of arguments of a function
   (only reasonable to call in a first-order setting) *)
let expected_fo_args f =
  let vars, args, _ = Ty.as_poly_sig (Cst.ty f) in
  List.length vars, List.length args

(* Split arguments of a function/constructor application *)
let split_fo_args poly n_ty n_t args =
  let n_args = List.length args in
  match poly with
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
  | `Expr f -> Expr (_wrap2 env ast f ast args)
  | `Tags f -> Tags (_wrap2 env ast f ast args)

(* Wrapper around record creation *)
let create_record env ast l =
  _wrap env ast Term.record l

let create_record_with env ast t l =
  _wrap2 env ast Term.record_with t l

let create_record_access env ast t field =
  _wrap2 env ast Field.apply field t

let make_eq env ast_term a b =
  _wrap2 env ast_term Term.eq a b

let mk_quant env ast mk vars_lists body =
  (* Emit warnings for quantified variables that are unused *)
  let fv_set = Term.fv body in
  List.iter (List.iter (fun v ->
      if not (Var.Set.mem v fv_set) then _unused_var env v
    )) vars_lists;
  (* Create the quantified formula *)
  _wrap2 env ast (List.fold_right mk) vars_lists body

let infer env ast s args s_ast =
  if Id.(s.ns = Var) then
    _error env (Ast ast) Infer_type_variable;
  match env.expect, env.infer_base with
  | Nothing, _ -> None
  | Type, _ ->
    let n = List.length args in
    let ret = Ty.cst (Id.full_name s) n in
    let res = Fun ret in
    env.infer_hook env res;
    decl_const env (Ast ast) s ret (Inferred (env.file, s_ast));
    Some res
  | Typed _, None -> None
  | Typed ty, Some base ->
    let n = List.length args in
    let ret = Term.cst (Id.full_name s) [] (Misc.Lists.replicate n base) ty in
    let res = Fun ret in
    env.infer_hook env res;
    decl_const env (Ast ast) s ret (Inferred (env.file, s_ast));
    Some res


(* Tag application *)
(* ************************************************************************ *)

let apply_tag env ast tag v res =
  match (res : res) with
  | Tags _ -> _error env (Ast ast) Cannot_tag_tag
  | Expr e -> Term.tag e tag v

(* Expression parsing *)
(* ************************************************************************ *)

let expect_base env =
  match env.infer_base with
  | None -> env
  | Some ty -> expect env (Typed ty)

let expect_prop env =
  expect env (Typed Ty.prop)

let rec parse_expr env t =
  match parse env t with
  | Expr e -> e
  | Tags _ as ret -> _expected env "an expression" t (Some ret)

and parse_ty env t =
  match parse env t with
  | Expr e when (View.Classify.view e = Type) -> e
  | res -> _expected env "a type" t (Some res)

and parse_term env t =
  match parse env t with
  | Expr e when (View.Classify.view e = Term) -> e
  | res -> _expected env "a term" t (Some res)

and parse_prop env ast =
  match parse (expect_prop env) ast with
  | Expr t ->
    if Ty.(equal prop) (Term.ty t) then t
    else _type_mismatch env t Ty.prop ast
  | Tags _ as res ->
    _expected env "a proposition" ast (Some res)

and parse (env : env) t : res =
  let res : res = match t with

    (* Ttype & builtin types *)
    | { Ast.term = Ast.Builtin Ast.Ttype; _ } ->
      Expr (Ty.type_)
    | { Ast.term = Ast.Builtin Ast.Prop; _ } ->
      Expr Ty.prop

    (* Wildcards should only occur in place of types *)
    | { Ast.term = Ast.Builtin Ast.Wildcard; _ } ->
      Expr (Ty.wildcard ())

    (* Basic formulas *)
    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.True; _ }, []); _ }
    | { Ast.term = Ast.Builtin Ast.True; _ } ->
      Expr Term._true

    | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.False; _ }, []); _ }
    | { Ast.term = Ast.Builtin Ast.False; _ } ->
      Expr Term._false

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.And; _ }, l); _ } ->
      Expr (_wrap env t Term._and (List.map (parse_prop env) l))

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Or; _ }, l); _ } ->
      Expr (_wrap env t Term._or (List.map (parse_prop env) l))

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Xor; _}, l); _ } as t ->
      begin match l with
        | [p; q] ->
          let f = parse_prop env p in
          let g = parse_prop env q in
          Expr (_wrap2 env t Term.xor f g)
        | _ -> _bad_op_arity env "xor" 2 (List.length l) t
      end

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Nand; _}, l); _ } as t ->
      begin match l with
        | [p; q] ->
          let f = parse_prop env p in
          let g = parse_prop env q in
          Expr (_wrap2 env t Term.nand f g)
        | _ -> _bad_op_arity env "nand" 2 (List.length l) t
      end

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Nor; _}, l); _ } as t ->
      begin match l with
        | [p; q] ->
          let f = parse_prop env p in
          let g = parse_prop env q in
          Expr (_wrap2 env t Term.nor f g)
        | _ -> _bad_op_arity env "nor" 2 (List.length l) t
      end

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Imply; _ }, l); _ } as t ->
      begin match l with
        | [p; q] ->
          let f = parse_prop env p in
          let g = parse_prop env q in
          Expr (_wrap2 env t Term.imply f g)
        | _ -> _bad_op_arity env "=>" 2 (List.length l) t
      end

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Implied; _ }, l); _ } as t ->
      begin match l with
        | [q; p] ->
          let f = parse_prop env p in
          let g = parse_prop env q in
          Expr (_wrap2 env t Term.imply f g)
        | _ -> _bad_op_arity env "<=" 2 (List.length l) t
      end

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Equiv; _}, l); _ } as t ->
      begin match l with
        | [p; q] ->
          let f = parse_prop env p in
          let g = parse_prop env q in
          Expr (_wrap2 env t Term.equiv f g)
        | _ -> _bad_op_arity env "<=>" 2 (List.length l) t
      end

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Not; _}, l); _ } as t ->
      begin match l with
        | [p] -> Expr (_wrap env t Term.neg (parse_prop env p))
        | _ -> _bad_op_arity env "not" 1 (List.length l) t
      end

    (* Binders *)
    | { Ast.term = Ast.Binder (Ast.All, _, _); _ } ->
      parse_quant parse_expr Term.all Ast.All env t [] t

    | { Ast.term = Ast.Binder (Ast.Ex, _, _); _ } ->
      parse_quant parse_prop Term.ex Ast.Ex env t [] t

    | { Ast.term = Ast.Binder (Ast.Fun, _, _); _ } ->
      parse_quant parse_prop Term.lambda Ast.Fun env t [] t

    | { Ast.term = Ast.Binder (Ast.Pi, _, _); _ } ->
      parse_pi env t

    | { Ast.term = Ast.Binder (Ast.Arrow, args, ret); _ } ->
      parse_arrow env t args ret

    (* Pattern matching *)
    | { Ast.term = Ast.Match (scrutinee, branches); _ } ->
      parse_match env t scrutinee branches

    (* (Dis)Equality *)
    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Eq; _ }, l); _ } as t ->
      begin match l with
        | [a; b] ->
          let env = expect_base env in
          let t1 = parse_term env a in
          let t2 = parse_term env b in
          Expr (make_eq env t t1 t2)
        | _ -> _bad_op_arity env "=" 2 (List.length l) t
      end

    | { Ast.term = Ast.App ({Ast.term = Ast.Builtin Ast.Distinct; _}, args); _ } ->
      let l' = List.map (parse_term env) args in
      Expr (_wrap env t Term.distinct l')

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

    (* Local bindings *)
    | { Ast.term = Ast.Binder (Ast.Let, vars, f); _ } ->
      parse_let env [] f vars

    (* Type annotations *)
    | { Ast.term = Ast.Colon (a, expected); _ } ->
      parse_ensure env a expected

    (* Variables, constants, and builtins,
       routed through the application typechecking *)
    | { Ast.term = Ast.Symbol _; _ } as ast ->
      parse_app env ast ast []
    | { Ast.term = Ast.Builtin _; _ } as ast ->
      parse_app env ast ast []

    (* General case: application *)
    | { Ast.term = Ast.App (f, l); _ } as ast ->
      parse_app env ast f l

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
    begin match parse (expect env Nothing) a with
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

and parse_typed_var env = function
  | { Ast.term = Ast.Colon ({ Ast.term = Ast.Symbol s; _ }, e); _ } ->
    let ty = parse_expr env e in
    begin match View.Classify.view ty with
      | Kind -> assert false (* this would be weird, really *)
      | Ttype -> `Ty, s, Ty.var (Id.full_name s)
      | Type -> `Term, s, Var.mk (Id.full_name s) ty
      | Term | Other -> _expected env "a type (or Ttype)" e (Some (Expr ty))
    end
  | { Ast.term = Ast.Symbol s; _ } ->
    begin match env.expect with
      | Type -> `Ty, s, Ty.var (Id.full_name s)
      | Typed ty -> `Term, s, Var.mk (Id.full_name s) ty
      | Nothing -> `Term, s, Var.mk (Id.full_name s) (Ty.wildcard ())
    end
  | t -> _expected env "a typed variable" t None

and parse_pi env ast =
  match env.quants with
  | Dependant | Prenex_types | Only_types ->
    let env = { env with quants = Only_types; } in
    parse_quant parse_expr Ty.pi Ast.Pi env ast [] ast
  | Only_terms | No_quantifiers ->
    _error env (Ast ast) Forbidden_quantifier

and parse_quant_vars env l =
  let env, l =
    List.fold_left (fun (env, acc) v ->
        let kind, id, v' = parse_typed_var env v in
        let v'', env = add_type_var env id v' v in
        env, (kind, v'') :: acc
      ) (env, []) l
  in
  env, List.rev l

and split_quant_vars env ast l =
  let rec aux kind acc = function
    | (kind', v) :: r when kind = kind' ->
      aux kind (v :: acc) r
    | l -> kind, List.rev acc, l
  in
  let rec accumulate env acc = function
    | [] -> env, List.rev acc
    | ((kind, _) :: _) as l ->
      let kind, l, r = aux kind [] l in
      let env = match env.quants, kind with
        (* Sucess cases *)
        | Only_types, `Ty -> env
        | Only_terms, `Term -> env
        | Prenex_types, `Ty -> env
        | Prenex_types, `Term -> { env with quants = Only_terms; }
        | Dependant, (`Ty | `Term) -> env
        (* TODO: for prenex_types, store location of the first/last
           term quantification to raise an error saying dependant
           quantification is forbidden *)
        | No_quantifiers, _ ->
          _error env (Ast ast) Forbidden_quantifier
        | Only_terms, `Ty ->
          _error env (Ast ast) Forbidden_quantifier
        | Only_types, `Term ->
          _error env (Ast ast) Forbidden_quantifier
      in
      accumulate env (l :: acc) r
  in
  accumulate env [] l

and parse_quant parse_body mk b env ast acc = function
  | { Ast.term = Ast.Binder (b', vars, f); _ } when b = b' ->
    let env, l = parse_quant_vars env vars in
    parse_quant parse_body mk b env ast (l @ acc) f
  | body_ast ->
    let env, l = split_quant_vars env ast acc in
    let body = parse_body env body_ast in
    let f = mk_quant env ast mk l body in
    Expr f

and parse_match env ast scrutinee branches =
  let t = parse_term env scrutinee in
  let l = List.map (parse_branch (Term.ty t) env) branches in
  Expr (_wrap2 env ast Term.pattern_match t l)

and parse_branch ty env (pattern, body) =
  let p, env = parse_pattern ~depth:0 ty env pattern in
  let b = parse_term env body in
  (p, b)

and parse_pattern ~depth ty env t =
  match t with
  | { Ast.term = Ast.Symbol s; _ } as ast_s ->
    parse_pattern_app ~depth ty env t ast_s s []
  | { Ast.term = Ast.App (
      ({ Ast.term = Ast.Symbol s; _ } as ast_s), args); _ } ->
    parse_pattern_app ~depth ty env t ast_s s args
  | _ -> _expected env "pattern" t None

and parse_pattern_app ~depth ty env ast ast_s s args =
  match env.pattern with
  | Shallow when depth > 0 ->
    parse_pattern_var ~shallow:true ty env ast_s s args
  | _ ->
    begin match find_bound env s with
      | `Cstr c -> parse_pattern_app_cstr ~depth ty env ast c args
      | _ -> parse_pattern_var ~shallow:false ty env ast_s s args
    end

and parse_pattern_var ~shallow ty env ast s = function
  | [] ->
    let v = Var.mk (Id.full_name s) ty in
    let v, env = add_term_var env s v ast in
    Term.of_var v, env
  | _ ->
    let msg = Format.asprintf "a variable %s"
        (if shallow then "(patterns must be shallow)" else "or an ADT constructor")
    in
    _expected env msg ast None

and parse_pattern_app_cstr ~depth ty env t c args =
  let n_ty, n_t = Cstr.arity c in
  let ty_l, t_l =
    match split_fo_args env.poly_pat n_ty n_t args with
    | `Ok (l, l') -> l, l'
    | `Bad_arity (expected, actual) ->
      _bad_cst_arity env c expected actual t
  in
  (* We can't allow binding new type variables here *)
  let ty_args = List.map (parse_ty env) ty_l in
  (* Compute the expected types of arguments *)
  let ty_arity = _wrap3 env t Cstr.pattern_arity c ty ty_args in
  (* Pattern args are allowed to introduce new variables *)
  let t_args, env = parse_pattern_app_cstr_args ~depth env t_l ty_arity in
  let res = _wrap2 env t Cstr.apply c (ty_args @ t_args) in
  res, env

and parse_pattern_app_cstr_args ~depth env args args_ty =
  let l, env =
    List.fold_left2 (fun (l, env) arg ty ->
        let arg, env = parse_pattern ~depth:(depth + 1) ty env arg in
        (arg :: l, env)
      ) ([], env) args args_ty
  in
  List.rev l, env

and parse_let env acc f = function
  | [] -> (* TODO: use continuation to avoid stack overflow on packs of let-bindings ? *)
    let l = List.rev acc in
    begin match parse env f with
      | Expr t -> Expr (Term.letin l t)
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
        let v = Var.mk (Id.full_name s) (Term.ty t) in
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
    Expr (create_record env ast l')

and parse_record_with env ast = function
  | [] ->
    _expected env "term" ast None
  | t :: l ->
    let t' = parse_term env t in
    let l' = List.map (parse_record_field_binding env) l in
    Expr (create_record_with env ast t' l')

and parse_record_access env ast = function
  | [ t; f ] ->
    let t = parse_term env t in
    let field = parse_record_field env f in
    Expr (create_record_access env ast t field)
  | l ->
    _bad_op_arity env "field record access" 2 (List.length l) ast

and parse_app env ast f_ast args =
  match env.order with
  | Signature order ->
    parse_app { env with order; } ast f_ast args
  | First_order ->
    begin match f_ast with
      | { Ast.term = Ast.Symbol s; _ } ->
        parse_app_direct env ast s f_ast args
      | _ ->
        _error env (Ast ast) Higher_order_application
    end
  | Higher_order ->
    parse_app_ho_args env ast f_ast [args]

and parse_app_ho_args env ast f_ast args_lists =
  match f_ast with
  | { Ast.term = Ast.Symbol s; _ } ->
    parse_app_direct env ast s f_ast (List.concat args_lists)
  | { Ast.term = Ast.App (f', args); _ } ->
    parse_app_ho_args env ast f' (args :: args_lists)
  | _ ->
    let f = parse_expr env f_ast in
    parse_app_ho env ast f (List.concat args_lists)

and parse_app_direct env ast s s_ast args =
  match find_bound env s with
  | `Var v -> parse_app_var env ast v args
  | `Letin (_, v, _) -> parse_app_var env ast v args
  | `Cst c -> parse_app_cst env ast c args
  | `Cstr c -> parse_app_cst env ast c args
  | `Field _f -> _expected env "not a field name" s_ast None
  | `Builtin b -> builtin_apply env b ast args
  | `Not_found ->
    begin match infer env ast s args s_ast with
      | Some Fun f -> parse_app_cst env ast f args
      | None -> _cannot_find env ast s
    end

and parse_app_var env ast v args =
  match env.order, args with
  | Signature order, _ -> parse_app_var { env with order; } ast v args
  | First_order, [] -> Expr (Term.of_var v)
  | First_order, _ :: _ -> _var_app env v ast
  | Higher_order, _ -> parse_app_ho env ast (Term.of_var v) args

and parse_app_cst env ast f args =
  match env.order with
  | Signature order -> parse_app_cst { env with order; } ast f args
  | First_order -> parse_app_fo env ast f args
  | Higher_order -> parse_app_ho env ast (Term.of_cst f) args

and parse_app_fo env ast f args =
  let n_ty, n_t = expected_fo_args f in
  let ty_l, t_l =
    match split_fo_args env.poly_app n_ty n_t args with
    | `Ok (l, l') -> l, l'
    | `Bad_arity (expected, actual) ->
      _bad_cst_arity env f expected actual ast
  in
  let ty_args = List.map (parse_ty env) ty_l in
  let t_args = List.map (parse_term env) t_l in
  Expr (_wrap3 env ast Term.apply_fo f ty_args t_args)

and parse_app_ho env ast f args =
  let t_args = List.map (parse_expr env) args in
  Expr (_wrap2 env ast Term.apply f t_args)

and parse_ite env ast = function
  | [c; a; b] ->
    let cond = parse_prop env c in
    let then_t = parse_term env a in
    let else_t = parse_term env b in
    Expr (_wrap3 env ast Term.ite cond then_t else_t)
  | args ->
    _bad_op_arity env "#ite" 3 (List.length args) ast

and parse_ensure env a expected =
  let t = parse_term env a in
  let ty = parse_ty env expected in
  Expr (Term.ensure t ty)

and parse_builtin env ast b args =
  match env.builtins env (Builtin b) with
  | `Not_found -> _unknown_builtin env ast b
  | #builtin_res as b -> builtin_apply env b ast args

and parse_type_var env t =
  match parse_typed_var (expect ~force:true env Type) t with
  | `Ty, id, v -> (id, v, t)
  | `Term, _, v -> _expected env "type variable" t (Some (Expr (Term.of_var v)))

and parse_arrow env ast args body =
  match args with
  | [{ Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Product; _}, l); _ }] ->
    parse_arrow_type env ast l body
  | _ -> parse_arrow_type env ast args body

and parse_arrow_type env ast args body =
  let env =
    match env.order with
    | Higher_order -> env
    | Signature order -> { env with order; }
    | First_order -> assert false (* TODO: proper error *)
  in
  let args = List.map (parse_expr env) args in
  let body = parse_expr env body in
  build_arrow env ast args body

and build_arrow env ast args body =
  match View.Classify.view body with
  | Ttype -> build_arrow_ttype env ast args body
  | Type -> build_arrow_type env ast args body
  | _ -> _expected env "a type or Ttype" ast (Some (Expr body))

and build_arrow_ttype env ast args body =
  let ret = Ty.arrow args body in
  let check_fv = lazy (
    Var.Set.iter (fun v ->
        _error env (Ast ast) (Type_var_in_type_constructor v)
      ) (Ty.fv ret)
  ) in
  (* Check that the type being built is reasonable *)
  List.iter (fun arg ->
      match View.Classify.view arg, env.quants with
      (* ok cases *)
      | Ttype, _
      | Type, Dependant -> Lazy.force check_fv
      (* Errors *)
      | Type, _ -> assert false (* TODO: add proper error *)
      | _, _ -> _expected env "a type or Ttype" ast (Some (Expr arg))
    ) args;
  (* Build the arrow, and check the free_variables *)
  Expr ret

and build_arrow_type env ast args body =
  (* Check that the type being built is reasonable *)
  List.iter (fun arg ->
      match View.Classify.view arg with
      | Type -> ()
      | _ -> _expected env "a type" ast (Some (Expr arg))
    ) args;
  let ret = Ty.arrow args body in
  Expr ret

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
        let c = Cst.mk (Id.full_name id) n in
        List.iter (fun (Any (tag, v)) -> Cst.tag c tag v) tags;
        id, `Type_decl c
      | `Fun_ty (vars, args, ret) ->
        let f = Cst.mk (Id.full_name id) vars args ret in
        List.iter (fun (Any (tag, v)) -> Cst.tag f tag v) tags;
        id, `Term_decl f
    end
  | Record { id; vars; _ }
  | Inductive { id; vars; _ } ->
    let n = List.length vars in
    let c = Cst.mk (Id.full_name id) n in
    List.iter (fun (Any (tag, v)) -> Cst.tag c tag v) tags;
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

