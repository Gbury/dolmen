
module M = Map.Make(Dolmen.Std.Id)

(* Definitions by Substitution *)
(* ************************************************************************ *)

module type Subst_arg = sig

  type ty
  type ty_var

  type term
  type term_var

  val ty_subst :
    (ty_var * ty) list -> ty -> ty
  val term_subst :
    (ty_var * ty) list -> (term_var * term) list -> term -> term

end

module Subst
    (Type : Tff_intf.S)
    (T : Subst_arg with type ty := Type.Ty.t
                    and type ty_var := Type.Ty.Var.t
                    and type term := Type.T.t
                    and type term_var := Type.T.Var.t) = struct

  let take_drop n l =
    let rec aux acc n = function
      | r when n <= 0 -> List.rev acc, r
      | [] -> raise (Invalid_argument "take_drop")
      | x :: r -> aux (x :: acc) (n - 1) r
    in
    aux [] n l

  let key = Dolmen.Std.Tag.create ()

  let get_defs env =
    match Type.get_global_custom env key with
    | None -> M.empty
    | Some m -> m

  let define_ty env id vars body =
    let map = get_defs env in
    let m = M.add id (`Ty (vars, body)) map in
    Type.set_global_custom env key m

  let define_term env id vars args body =
    let map = get_defs env in
    let m = M.add id (`Term (vars, args, body)) map in
    Type.set_global_custom env key m

  let parse env symbol =
    match (symbol : Type.symbol) with
    | Id id ->
      begin match M.find id (get_defs env) with
        | `Ty (vars, body) ->
          Type.builtin_ty (Base.make_opn (List.length vars)
                 (module Type) env symbol (fun _ args ->
                     let ty_args = List.map (Type.parse_ty env) args in
                     let l = List.map2 (fun x y -> x, y) vars ty_args in
                     T.ty_subst l body
                   ))
        | `Term (ty_vars, t_vars, body) ->
          Type.builtin_term (fun ast args ->
              let n_args = List.length args in
              let n_ty = List.length ty_vars in
              let n_t = List.length t_vars in
              let ty_l, t_l =
                if n_args = n_ty + n_t then
                  take_drop n_ty args
                else begin
                  Type._error env (Ast ast)
                    (Type.Bad_op_arity (symbol, [n_ty + n_t], n_args))
                end
              in
              let ty_l = List.map2 (fun x y -> x, y) ty_vars
                  (List.map (Type.parse_ty env) ty_l) in
              let t_l = List.map2 (fun x y -> x, y) t_vars
                  (List.map (Type.parse_term env) t_l) in
              T.term_subst ty_l t_l body
            )
        | exception Not_found -> `Not_found
      end
    | _ -> `Not_found

end

(* Definitions by Declaration *)
(* ************************************************************************ *)


module Declare(Type : Tff_intf.S) = struct

  let key = Dolmen.Std.Tag.create ()

  let get_defs env =
    match Type.get_global_custom env key with
    | None -> M.empty
    | Some m -> m

  let add_definition env id def =
    let map = get_defs env in
    let m = M.add id def map in
    Type.set_global_custom env key m

  let define_ty env id vars _body =
    let path = Type.cst_path env (Dolmen.Std.Id.name id) in
    let c = Type.Ty.Const.mk path (List.length vars) in
    let () = add_definition env id (`Ty c) in
    c

  let define_term env id vars args body =
    let ret_ty = Type.T.ty body in
    let args_ty = List.map Type.T.Var.ty args in
    let path = Type.cst_path env (Dolmen.Std.Id.name id) in
    let ty = Type.Ty.pi vars (Type.Ty.arrow args_ty ret_ty) in
    let c = Type.T.Const.mk path ty in
    let () = add_definition env id (`Term c) in
    c

  let parse env symbol =
    match (symbol : Type.symbol) with
    | Id id ->
      begin match M.find id (get_defs env) with
        | `Ty c -> Type.builtin_ty (fun ast args ->
            Type.unwrap_ty env ast (Type.parse_app_ty_cst env ast c args))
        | `Term c -> Type.builtin_term (fun ast args ->
            Type.unwrap_term env ast (Type.parse_app_term_cst env ast c args))
        | exception Not_found -> `Not_found
      end
    | Builtin _ -> `Not_found

end

