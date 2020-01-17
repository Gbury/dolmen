
open Dolmen

(* Definitions by Substitution *)
(* ************************************************************************ *)

module type Expand_arg = sig

  type t

end

module Expand
    (Type : Tff_intf.S)
    (T : Expand_arg with type t = Type.T.t) = struct

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

