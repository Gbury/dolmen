
open Dolmen

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
    (T : Subst_arg with type ty = Type.Ty.t
                    and type ty_var = Type.Ty.Var.t
                    and type term = Type.T.t
                    and type term_var = Type.T.Var.t) = struct

  module H = Hashtbl.Make(Id)

  let take_drop n l =
    let rec aux acc n = function
      | r when n <= 0 -> List.rev acc, r
      | [] -> raise (Invalid_argument "take_drop")
      | x :: r -> aux (x :: acc) (n - 1) r
    in
    aux [] n l

  let definitions = H.create 13

  let define_ty id vars body =
    H.add definitions id (`Ty (vars, body))

  let define_term id vars args body =
    H.add definitions id (`Term (vars, args, body))

  let parse env ast symbol args =
    match (symbol : Type.symbol) with
    | Id id ->
      begin match H.find definitions id with
        | `Ty (vars, body) ->
          Base.make_opn (List.length vars)
            (module Type) env ast (Id.full_name id) args (fun args ->
                let ty_args = List.map (Type.parse_ty env) args in
                let l = List.map2 (fun x y -> x, y) vars ty_args in
                Type.Ty (T.ty_subst l body)
              )
        | `Term (ty_vars, t_vars, body) ->
          let n_args = List.length args in
          let n_ty = List.length ty_vars in
          let n_t = List.length t_vars in
          let ty_l, t_l =
            if n_args = n_ty + n_t then
              take_drop n_ty args
            else begin
              let err = Type.Bad_op_arity (Id.full_name id, n_ty, n_ty) in
              raise (Type.Typing_error (err, env, ast))
            end
          in
          let ty_l = List.map2 (fun x y -> x, y) ty_vars
              (List.map (Type.parse_ty env) ty_l) in
          let t_l = List.map2 (fun x y -> x, y) t_vars
              (List.map (Type.parse_term env) t_l) in
          Some (Type.Term (T.term_subst ty_l t_l body))
        | exception Not_found -> None
      end
    | Builtin _ -> None

end

(* Definitions by Declaration *)
(* ************************************************************************ *)


module Declare(Type : Tff_intf.S) = struct

  module H = Hashtbl.Make(Id)

  let definitions = H.create 13

  let define_ty id vars _body =
    let c = Type.Ty.Const.mk (Id.full_name id) (List.length vars) in
    H.add definitions id (`Ty c)

  let define_term id vars args body =
    let ret_ty = Type.T.ty body in
    let args_ty = List.map Type.T.Var.ty args in
    let c = Type.T.Const.mk (Id.full_name id) vars args_ty ret_ty in
    H.add definitions id (`Term c)

  let parse env ast symbol args =
    match (symbol : Type.symbol) with
    | Id id ->
      begin match H.find definitions id with
        | `Ty c -> Some (Type.parse_app_ty env ast c args)
        | `Term c -> Some (Type.parse_app_term env ast c args)
        | exception Not_found -> None
      end
    | Builtin _ -> None

end

