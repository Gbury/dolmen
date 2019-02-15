
open Dolmen

(* Smtlib arrays *)
(* ************************************************************************ *)

module Smtlib = struct

  module type Ty = sig
    type t
    val array : t -> t -> t
  end

  module type T = sig
    type t
    val select : t -> t -> t
    val store : t -> t -> t -> t
  end

  module Tff
      (Type : Tff_intf.S)
      (Ty : Ty with type t = Type.Ty.t)
      (T : T with type t = Type.T.t) = struct

    let parse env ast s args =
      match s with
      | Type.Id { Id.name = "Array"; ns = Id.Sort } ->
        Base.make_op2 (module Type) env ast "Array" args (fun (src, dst) ->
            let src_ty = Type.parse_ty env src in
            let dst_ty = Type.parse_ty env dst in
            Type.Ty (Ty.array src_ty dst_ty)
          )
      | Type.Id { Id.name = "select"; ns = Id.Term } ->
        Base.make_op2 (module Type) env ast "select" args (fun (arr, idx) ->
            let arr_t = Type.parse_term env arr in
            let idx_t = Type.parse_term env idx in
            Type.Term (T.select arr_t idx_t)
          )
      | Type.Id { Id.name = "store"; ns = Id.Term } ->
        Base.make_op3 (module Type) env ast "store" args (fun (arr, idx, value) ->
            let arr_t = Type.parse_term env arr in
            let idx_t = Type.parse_term env idx in
            let val_t = Type.parse_term env value in
            Type.Term (T.store arr_t idx_t val_t)
          )
      | _ -> None

  end

end
