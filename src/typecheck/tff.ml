
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

open Dolmen
module type S = Tff_intf.S

(* Common types *)
(* ************************************************************************ *)

(* The type of reasons for constant typing *)
type reason =
  | Inferred of ParseLocation.t
  | Declared of ParseLocation.t

type ('ty_const, 'term_cstr, 'term_const) binding = [
  | `Not_found
  | `Ty of 'ty_const * reason
  | `Cstr of 'term_cstr * reason
  | `Term of 'term_const * reason
]

(* Warnings module signature *)
(* ************************************************************************ *)

module type Warn = sig

  type ty
  type ty_var
  type ty_const

  type term
  type term_var
  type term_cstr
  type term_const

  val shadow : Id.t ->
    (ty_const, term_cstr, term_const) binding ->
    (ty_const, term_cstr, term_const) binding ->
    unit

  val unused_ty_var : ParseLocation.t -> ty_var -> unit
  val unused_term_var : ParseLocation.t -> term_var -> unit

  val error_in_attribute : ParseLocation.t -> exn -> unit

  val not_found : Id.t -> (int -> Id.t list) -> unit

  val superfluous_destructor :
    ParseLocation.t -> Id.t -> Id.t -> term_const -> unit

end

(* Typechecking functor *)
(* ************************************************************************ *)

module Make
    (Tag: Dolmen_intf.Tag.S)
    (Ty: Dolmen_intf.Type.Tff
     with type 'a tag := 'a Tag.t)
    (T: Dolmen_intf.Term.Tff
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type 'a tag := 'a Tag.t)
    (W : Warn
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type term := T.t
      and type term_var := T.Var.t
      and type term_cstr := T.Cstr.t
      and type term_const := T.Const.t
    )
= struct

  (* Module aliases *)
  (* ************************************************************************ *)

  module Tag = Tag
  module Ty = Ty
  module T = T

  (* Log&Module Init *)
  (* ************************************************************************ *)

  let default_loc = ParseLocation.mk "<?>" 0 0 0 0

  let or_default_loc = function
    | Some loc -> loc
    | None -> default_loc

  let get_loc t =
    match t.Term.loc with Some l -> l | None -> default_loc

  (* Module alias to avoid confusing untyped Terms and typed terms *)
  module Ast = Term

  (** Some maps and hashtables *)
  module E = Map.Make(Ty.Var)
  module F = Map.Make(T.Var)
  module R = Hashtbl.Make(Ty.Const)
  module S = Hashtbl.Make(T.Const)
  module U = Hashtbl.Make(T.Cstr)

  (* Convenience functions *)
  (* ************************************************************************ *)

  let replicate n x =
    let rec aux x acc n =
      if n <= 0 then acc else aux x (x :: acc) (n - 1)
    in
    aux x [] n

  let take_drop n l =
    let rec aux acc n = function
      | r when n <= 0 -> List.rev acc, r
      | [] -> raise (Invalid_argument "take_drop")
      | x :: r -> aux (x :: acc) (n - 1) r
    in
    aux [] n l

  (* Fuzzy search maps *)
  (* ************************************************************************ *)

  module M = struct

    module S = Spelll
    module I = S.Index

    (** We use fuzzy maps in order to give suggestions in case of typos.
        Since such maps are not trivial to extend to Dolmen identifiers,
        we map strings (identifier names) to list of associations. *)
    type 'a t = (Id.t * 'a) list I.t

    let eq id (x, _) = Id.equal id x

    let empty = I.empty

    let get t id =
      let s = Id.(id.name) in
      match List.of_seq (I.retrieve ~limit:0 t s) with
      | [l] -> l
      | [] -> []
      | _ -> assert false

    let mem id t =
      List.exists (eq id) (get t id)

    let find id t =
      snd @@ List.find (eq id) (get t id)

    let add id v t =
      let l = get t id in
      let l' =
        if List.exists (eq id) (get t id) then l
        else (id, v) :: l
      in
      I.add t Id.(id.name) l'

    (** Return a list of suggestions for an identifier. *)
    let suggest ~limit id t =
      let s = Id.(id.name) in
      let l = List.of_seq (I.retrieve ~limit t s) in
      List.flatten @@ List.map (List.map fst) l

  end

  (* Fuzzy search hashtables *)
  (* ************************************************************************ *)

  module H = struct

    (** Fuzzy hashtables are just references to fuzzy maps. *)
    let create () = ref M.empty

    let find r id = M.find id !r

    let suggest r id = M.suggest id !r

    let add r id v =
      r := M.add id v !r

  end

  (* Types *)
  (* ************************************************************************ *)


  (* The type of potentially expected result type for parsing an expression *)
  type expect =
    | Nothing
    | Type
    | Typed of Ty.t

  (* The type returned after parsing an expression. *)
  type tag = Any : 'a Tag.t * 'a -> tag

  type res =
    | Ttype   : res
    | Ty      : Ty.t -> res
    | Term    : T.t -> res
    | Tags    : tag list -> res

  type inferred =
    | Ty_fun of Ty.Const.t
    | Term_fun of T.Const.t

  (* Wrapper around potential function symbols in Dolmen *)
  type symbol =
    | Id of Id.t
    | Builtin of Ast.builtin

  (* The local environments used for type-checking. *)
  type env = {

    (* Map from term variables to the reason of its type *)
    type_locs : reason E.t;
    term_locs : reason F.t;

    (* local variables (mostly quantified variables) *)
    type_vars : Ty.Var.t  M.t;
    term_vars : T.Var.t   M.t;

    (* The current builtin symbols *)
    builtins : builtin_symbols;

    (* Additional typing info *)
    expect      : expect;
    infer_base  : Ty.t option;
    infer_hook  : env -> inferred -> unit;
  }

  (* Builtin symbols, i.e symbols understood by some theories,
     but which do not have specific syntax, so end up as special
     cases of application. *)
  and builtin_symbols = env -> Ast.t -> symbol -> Ast.t list -> res option

  type 'a typer = env -> Ast.t -> 'a


  (* Global Environment *)
  (* ************************************************************************ *)

  (* Global identifier table; stores declared types and aliases.
     Global env           : association between dolmen ids and types/terms constants
     Ttype location table : stores reasons for typing of type constructors
     Const location table : stores reasons for typing of constants *)
  let global_env = H.create ()
  let ttype_locs = R.create 4013
  let const_locs = S.create 4013
  let cstrs_locs = U.create 4013

  let find_global id =
    try H.find global_env id
    with Not_found -> `Not_found

  let find_reason v =
    try match v with
    | `Not_found -> assert false
    | `Ty c -> R.find ttype_locs c
    | `Cstr c -> U.find cstrs_locs c
    | `Term c -> S.find const_locs c
    with Not_found -> assert false

  let with_reason reason = function
    | `Not_found -> `Not_found
    | `Ty c -> `Ty (c, reason)
    | `Cstr c -> `Cstr (c, reason)
    | `Term c -> `Term (c, reason)

  let add_global id reason v =
    begin match find_global id with
      | `Not_found -> ()
      | old ->
        W.shadow id
          (with_reason (find_reason old) old)
          (with_reason reason v)
    end;
    H.add global_env id v

  (* Symbol declarations *)
  let decl_ty_const id c reason =
    add_global id reason (`Ty c);
    R.add ttype_locs c reason

  let decl_term_const id c reason =
    add_global id reason (`Term c);
    S.add const_locs c reason

  let decl_term_cstr id c reason =
    add_global id reason (`Cstr c);
    U.add cstrs_locs c reason

  (* Exported wrappers *)
  let declare_ty_const id c loc = decl_ty_const id c (Declared loc)
  let declare_term_const id c loc = decl_term_const id c (Declared loc)


  (* Local Environment *)
  (* ************************************************************************ *)

  (* Make a new empty environment *)
  let empty_env
      ?(expect=Nothing) ?(infer_hook=(fun _ _ -> ())) ?infer_base builtins = {
    type_locs = E.empty;
    term_locs = F.empty;
    type_vars = M.empty;
    term_vars = M.empty;
    builtins; expect; infer_hook; infer_base;
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
  let add_type_var env id v loc =
    let v' =
      if M.mem id env.type_vars then
        Ty.Var.mk (new_ty_name ())
      else
        v
    in
    v', { env with
          type_vars = M.add id v' env.type_vars;
          type_locs = E.add v' (Declared loc) env.type_locs;
        }

  let add_type_vars env l =
    let l', env' = List.fold_left (fun (l, acc) (id, v, loc) ->
        let v', acc' = add_type_var acc id v loc in
        v' :: l, acc') ([], env) l in
    List.rev l', env'

  let add_term_var env id v loc =
    let v' =
      if M.mem id env.type_vars then
        T.Var.mk (new_term_name ()) (T.Var.ty v)
      else
        v
    in
    v', { env with
          term_vars = M.add id v' env.term_vars;
          term_locs = F.add v' (Declared loc) env.term_locs;
        }

  let find_var env name =
    try `Ty (M.find name env.type_vars)
    with Not_found ->
      begin
        try
          `Term (M.find name env.term_vars)
        with Not_found ->
          `Not_found
      end

  (* Typo suggestion *)
  (* ************************************************************************ *)

  let suggest ~limit env id =
    M.suggest ~limit id env.type_vars @
    M.suggest ~limit id env.term_vars @
    H.suggest ~limit global_env id

  (* Typing explanation *)
  (* ************************************************************************ *)

  let get_reason_loc = function Inferred l | Declared l -> l

(* TODO: uncomment this code
  exception Continue of Expr.term list

  let pp_reason fmt = function
    | Inferred loc -> Format.fprintf fmt "inferred at %a" Dolmen.ParseLocation.fmt loc
    | Declared loc -> Format.fprintf fmt "declared at %a" Dolmen.ParseLocation.fmt loc

  let rec explain ~full env fmt t =
    try
      begin match t with
        | { Expr.term = Expr.Var v } ->
          let reason = F.find v env.term_locs in
          Format.fprintf fmt "%a was %a\n" Expr.Print.id_ty v pp_reason reason
        | { Expr.term = Expr.Meta m } ->
          let f = Expr.Meta.def Expr.(m.meta_index) in
          Format.fprintf fmt "%a was defined by %a\n"
            Expr.Print.meta m Expr.Print.formula f
        | { Expr.term = Expr.App (f, _, l) } ->
          let reason = S.find const_locs f in
          Format.fprintf fmt "%a was %a\n" Expr.Print.const_ty f pp_reason reason;
          if full then raise (Continue l)
      end with
    | Not_found ->
      Format.fprintf fmt "Couldn't find a reason..."
    | Continue l ->
      List.iter (explain env ~full fmt) l
*)

  (* Exceptions *)
  (* ************************************************************************ *)

  (* Internal exception *)
  exception Found of Ast.t * res

  type err = ..

  type err +=
    | Infer_type_variable
    | Expected of string * res option
    | Bad_op_arity of string * int * int
    | Bad_ty_arity of Ty.Const.t * int
    | Bad_cstr_arity of T.Cstr.t * int * int
    | Bad_term_arity of T.Const.t * int * int
    | Var_application of T.Var.t
    | Ty_var_application of Ty.Var.t
    | Type_mismatch of T.t * Ty.t
    | Quantified_var_inference
    | Unhandled_builtin of Term.builtin
    | Cannot_tag_tag
    | Cannot_tag_ttype
    | Cannot_find of Id.t
    | Type_var_in_type_constructor
    | Missing_destructor of Id.t
    | Higher_order_application
    | Unbound_variables of Ty.Var.t list * T.Var.t list * T.t
    | Unhandled_ast

  (* Exception for typing errors *)
  exception Typing_error of err * env * Ast.t

  (* Exception for not well-dounded datatypes definitions. *)
  exception Not_well_founded_datatypes of Dolmen.Statement.decl list

    (* TOOD: uncomment this code
  (* Creating explanations *)
  let mk_expl preface env fmt t = ()
    match env.explain with
    | `No -> ()
    | `Yes ->
      Format.fprintf fmt "%s\n%a" preface (explain ~full:false env) t
    | `Full ->
       Format.fprintf fmt "%s\n%a" preface (explain ~full:true env) t
    *)

  (* Convenience functions *)
  let _infer_var env t =
    raise (Typing_error (Infer_type_variable, env, t))

  let _expected env s t res =
    raise (Typing_error (Expected (s, res), env, t))

  let _bad_op_arity env s n m t =
    raise (Typing_error (Bad_op_arity (s, n, m), env, t))

  let _bad_ty_arity env f n t =
    raise (Typing_error (Bad_ty_arity (f, n), env, t))

  let _bad_term_arity env f (n1, n2) t =
    raise (Typing_error (Bad_term_arity (f, n1, n2), env, t))

  let _bad_cstr_arity env c (n1, n2) t =
    raise (Typing_error (Bad_cstr_arity (c, n1, n2), env, t))

  let _ty_var_app env v t =
    raise (Typing_error (Ty_var_application v, env, t))

  let _var_app env v t =
    raise (Typing_error (Var_application v, env, t))

  let _type_mismatch env t ty ast =
    raise (Typing_error (Type_mismatch (t, ty), env, ast))

  let _cannot_infer_quant_var env t =
    raise (Typing_error (Quantified_var_inference, env, t))

  let _unused_type v env =
    W.unused_ty_var (get_reason_loc (E.find v env.type_locs)) v

  let _unused_term v env =
    W.unused_term_var (get_reason_loc (F.find v env.term_locs)) v

  let _unknown_builtin env ast b =
    raise (Typing_error (Unhandled_builtin b, env, ast))

  let _wrap env ast f arg =
    try f arg
    with T.Wrong_type (t, ty) ->
      _type_mismatch env t ty ast

  let _wrap2 env ast f a b =
    try f a b
    with T.Wrong_type (t, ty) ->
      _type_mismatch env t ty ast

  let _wrap3 env ast f a b c =
    try f a b c
    with T.Wrong_type (t, ty) ->
      _type_mismatch env t ty ast

  (* Wrappers for expression building *)
  (* ************************************************************************ *)

  (* Wrapper around type application *)
  let ty_apply env ast f args =
    if List.length args = Ty.Const.arity f then Ty.apply f args
    else _bad_ty_arity env f (Ty.Const.arity f) ast

  (* Wrapper around term application. *)
  let term_apply env ast f ty_args t_args =
    let n1, n2 = T.Const.arity f in
    if n1 = List.length ty_args &&
       n2 = List.length t_args then
      _wrap3 env ast T.apply f ty_args t_args
    else
      _bad_term_arity env f (n1, n2) ast

  let term_apply_cstr env ast c ty_args t_args =
    let n1, n2 = T.Cstr.arity c in
    if n1 = List.length ty_args &&
       n2 = List.length t_args then
      _wrap3 env ast T.apply_cstr c ty_args t_args
    else
      _bad_cstr_arity env c (n1, n2) ast

  let make_eq env ast_term a b =
    _wrap2 env ast_term T.eq a b

  let mk_quant env ast mk (ty_vars, t_vars) body =
    let fv_ty, fv_t = T.fv body in
    (* Check that all quantified variables are actually used *)
    List.iter (fun v ->
        if not @@ List.exists (Ty.Var.equal v) fv_ty then _unused_type v env
      ) ty_vars;
    List.iter (fun v ->
        if not @@ List.exists (T.Var.equal v) fv_t then _unused_term v env
      ) t_vars;
    (* Filter quantified variables from free_variables *)
    let fv_ty = List.filter (fun v ->
        List.exists (Ty.Var.equal v) ty_vars) fv_ty in
    let fv_t = List.filter (fun v ->
        List.exists (T.Var.equal v) t_vars) fv_t in
    (* Create the quantified formula *)
    _wrap3 env ast mk (fv_ty, fv_t) (ty_vars, t_vars) body

  let infer env ast s args loc =
    if Id.(s.ns = Var) then _infer_var env ast;
    match env.expect, env.infer_base with
    | Nothing, _ -> None
    | Type, _ ->
      let n = List.length args in
      let ret = Ty.Const.mk (Id.full_name s) n in
      let res = Ty_fun ret in
      env.infer_hook env res;
      decl_ty_const s ret (Inferred loc);
      Some res
    | Typed _, None -> None
    | Typed ty, Some base ->
      let n = List.length args in
      let ret = T.Const.mk (Id.full_name s) [] (replicate n base) ty in
      let res = Term_fun ret in
      env.infer_hook env res;
      decl_term_const s ret (Inferred loc);
      Some res

  (* Tag application *)
  (* ************************************************************************ *)

  let apply_tag env ast tag v = function
    | Ttype -> raise (Typing_error (Cannot_tag_ttype, env, ast))
    | Tags _ -> raise (Typing_error (Cannot_tag_tag, env, ast))
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

  let rec parse_expr (env : env) t =
    let res = match t with

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
        parse_quant T.all Ast.All env [] [] t

      | { Ast.term = Ast.Binder (Ast.Ex, _, _); _ } ->
        parse_quant T.all Ast.Ex env [] [] t

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
        parse_app env ast s []
      | { Ast.term = Ast.App ({ Ast.term = Ast.Symbol s; _ }, l); _ } as ast ->
        parse_app env ast s l

      (* If-then-else *)
      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin Ast.Ite; _}, l); _ } as ast ->
        parse_ite env ast l

      (* Builtin application not treated directly, but instead
         routed to a semantic extension through builtin_symbols. *)
      | { Ast.term = Ast.Builtin b; _ } as ast ->
        parse_builtin env ast b []
      | { Ast.term = Ast.App ({ Ast.term = Ast.Builtin b; _ }, l); _ } as ast ->
        parse_builtin env ast b l

      (* Local bindings *)
      | { Ast.term = Ast.Binder (Ast.Let, vars, f); _ } ->
        parse_let env [] f vars

      (* Explicitly catch higher-order application. *)
      | { Ast.term = Ast.App ({ Ast.term = Ast.App _; _ }, _); _ } as ast ->
        raise (Typing_error (Higher_order_application, env, ast))

      (* Other cases *)
      | ast -> raise (Typing_error (Unhandled_ast, env, ast))
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
        | exception (Typing_error (_, _, t) as exn) ->
          W.error_in_attribute (get_loc t) exn;
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
            let v'', acc' = add_type_var acc id v' (get_loc v) in
            (v'' :: l1, l2, acc')
          | `Term (id, v') ->
            let v'', acc' = add_term_var acc id v' (get_loc v) in
            (l1, v'' :: l2, acc')
      ) ([], [], env) l in
    List.rev ttype_vars, List.rev typed_vars, env'

  and parse_quant mk b env ttype_acc ty_acc = function
    | { Ast.term = Ast.Binder (b', vars, f); _ } when b = b' ->
      let ttype_vars, ty_vars, env' = parse_quant_vars (expect_base env) vars in
      parse_quant mk b env' (ttype_acc @ ttype_vars) (ty_acc @ ty_vars) f
    | ast ->
      let body = parse_prop env ast in
      let f = mk_quant env ast mk (ttype_acc, ty_acc) body in
      Term f

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
          let v', env' = add_term_var env s v (get_loc w) in
          parse_let env' ((v', t) :: acc) f r
        | t -> _expected env "variable binding" t None
      end

  and parse_app env ast s args =
    match find_var env s with
    | `Ty v ->
      if args = [] then Ty (Ty.of_var v)
      else _ty_var_app env v ast
    | `Term v ->
      if args = [] then Term (T.of_var v)
      else _var_app env v ast
    | `Not_found ->
      begin match find_global s with
        | `Ty f ->
          parse_app_ty env ast f args
        | `Cstr c ->
          parse_app_cstr env ast c args
        | `Term f ->
          parse_app_term env ast f args
        | `Not_found ->
          begin match env.builtins env ast (Id s) args with
            | Some res -> res
            | None ->
              begin match infer env ast s args (get_loc ast) with
                | Some Ty_fun f -> parse_app_ty env ast f args
                | Some Term_fun f -> parse_app_term env ast f args
                | None ->
                  W.not_found s (fun limit -> suggest ~limit env s);
                  raise (Typing_error (Cannot_find s, env, ast))
              end
            | exception T.Wrong_type (t, ty) ->
              _type_mismatch env t ty ast
          end
      end

  and parse_app_ty env ast f args =
    let l = List.map (parse_ty env) args in
    Ty (ty_apply env ast f l)

  and parse_app_term env ast f args =
    let n_args = List.length args in
    let n_ty, n_t = T.Const.arity f in
    let ty_l, t_l =
      if n_args = n_ty + n_t then take_drop n_ty args
      else _bad_term_arity env f (n_ty, n_t) ast
    in
    let ty_args = List.map (parse_ty env) ty_l in
    let t_args = List.map (parse_term env) t_l in
    Term (term_apply env ast f ty_args t_args)

  and parse_app_cstr env ast c args =
    let n_args = List.length args in
    let n_ty, n_t = T.Cstr.arity c in
    let ty_l, t_l =
      if n_args = n_ty + n_t then take_drop n_ty args
      else _bad_cstr_arity env c (n_ty, n_t) ast
    in
    let ty_args = List.map (parse_ty env) ty_l in
    let t_args = List.map (parse_term env) t_l in
    Term (term_apply_cstr env ast c ty_args t_args)

  and parse_ite env ast = function
    | [c; a; b] ->
      let cond = parse_prop env c in
      let then_t = parse_term env a in
      let else_t = parse_term env b in
      Term (_wrap3 env ast T.ite cond then_t else_t)
    | args ->
      _bad_op_arity env "#ite" 3 (List.length args) ast

  and parse_builtin env ast b args =
    match env.builtins env ast (Builtin b) args with
    | Some res -> res
    | None -> _unknown_builtin env ast b

  and parse_ty env ast =
    match parse_expr (expect env Type) ast with
    | Ty ty -> ty
    | res -> _expected env "type" ast (Some res)

  and parse_term env ast =
    match parse_expr (expect_base env) ast with
    | Term t -> t
    | res -> _expected env "term" ast (Some res)

  and parse_prop env ast =
    match parse_expr (expect_prop env) ast with
    | Term t -> t
    | res -> _expected env "term/prop" ast (Some res)

  let parse_ttype_var env t =
    match parse_var (expect ~force:true env Type) t with
    | `Ty (id, v) -> (id, v, get_loc t)
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
              raise (Typing_error (Type_var_in_type_constructor, env, h))
            | [] ->
              let aux n = function
                | (_, Ttype) -> n + 1
                | (ast, res) -> raise (Found (ast, res))
              in
              begin
                match List.fold_left aux 0 ty_args with
                | n -> `Ty_cstr n
                | exception Found (err, _) ->
                  raise (Typing_error (Type_var_in_type_constructor, env, err))
              end
          end
        | Ty ret ->
          let aux acc = function
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
    match (t : Statement.decl) with
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
    match (t : Statement.decl) with
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

  let rec check_well_founded l =
    match (l : Statement.decl list) with
    | [] -> ()
    | _ ->
      let has_progressed = ref false in
      let l' = List.filter (fun t ->
          let b = well_founded_aux l t in
          if b then has_progressed := true;
          not b
        ) l in
      if !has_progressed then
        check_well_founded l'
      else
        raise (Not_well_founded_datatypes l')

  let inductive env ty_cst { Statement.id; vars; cstrs; loc; _ } =
    let loc = or_default_loc loc in
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
            ty, Option.map Id.full_name dstr
          ) args
      ) cstrs_with_ids in
    (* Call the T module to define the adt and get the typed constructors
       and destructors. *)
    let defined_cstrs = T.define_adt ty_cst ty_vars cstrs_with_strings in
    (* Register the constructors and destructors in the global env. *)
    List.iter2 (fun (cid, pargs) (c, targs) ->
        let reason = Declared loc in
        decl_term_cstr cid c reason;
        List.iter2 (fun (t, _, dstr) (_, o) ->
            match dstr, o with
            | None, None -> ()
            | None, Some c -> W.superfluous_destructor loc id cid c
            | Some id, Some const -> decl_term_const id const reason
            | Some id, None ->
              raise (Typing_error (Missing_destructor id, env, t))
          ) pargs targs
      ) cstrs_with_ids defined_cstrs

  let decl env cst t =
    match cst, (t : Statement.decl) with
    | _, Abstract _ -> ()
    | `Term_decl _, Inductive _ -> assert false
    | `Type_decl c, Inductive i -> inductive env c i
    | `Term_decl _, Record _ -> assert false
    | `Type_decl _, Record _ -> (* TODO *) ()

  let decls env ?attr l =
    let tags = match attr with
      | None -> []
      | Some a -> parse_attr_and env a
    in
    (* Check well-foundedness *)
    check_well_founded l;
    (* First create (in the global env) the type const for each adt *)
    let l_decl = List.map (fun (t : Statement.decl) ->
        match t with
        | Abstract { id; ty; loc; } ->
          begin match parse_sig env ty with
            | `Ty_cstr n ->
              let c = Ty.Const.mk (Id.full_name id) n in
              List.iter (fun (Any (tag, v)) -> Ty.Const.tag c tag v) tags;
              decl_ty_const id c (Declared (or_default_loc loc));
              `Type_decl c
            | `Fun_ty (vars, args, ret) ->
              let f = T.Const.mk (Id.full_name id) vars args ret in
              List.iter (fun (Any (tag, v)) -> T.Const.tag f tag v) tags;
              decl_term_const id f (Declared (or_default_loc loc));
              `Term_decl f
          end
        | Record { id; vars; loc; _ }
        | Inductive { id; vars; loc; _ } ->
          let n = List.length vars in
          let c = Ty.Const.mk (Id.full_name id) n in
          List.iter (fun (Any (tag, v)) -> Ty.Const.tag c tag v) tags;
          decl_ty_const id c (Declared (or_default_loc loc));
          `Type_decl c
      ) l in
    (* Parse each definition
       TODO: parse (and thus define them with T) in the topological order
             defined by the well-founded check ? *)
    List.iter2 (decl env) l_decl l;
    (* Return the defined types *)
    l_decl

  (* High-level parsing functions *)
  (* ************************************************************************ *)

  let new_def env t ?attr id =
    let tags = match attr with
      | None -> []
      | Some a -> parse_attr_and env a
    in
    match parse_fun [] [] env t with
    | `Ty (ty_args, body) ->
      `Type_def (id, tags, ty_args, body)
    | `Term (ty_args, t_args, body) ->
      `Term_def (id, tags, ty_args, t_args, body)

  let parse env ast =
    let res = parse_prop env ast in
    match T.fv res with
    | [], [] -> res
    | tys, ts ->
      raise (Typing_error (Unbound_variables (tys, ts, res), env, ast))

end
