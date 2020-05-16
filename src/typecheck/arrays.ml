
open Dolmen

(* Smtlib arrays *)
(* ************************************************************************ *)

module Smtlib2 = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Array with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Array with type t := Type.T.t) = struct

    let parse _version env s =
      match s with
      | Type.Id { Id.name = "Array"; ns = Id.Sort } ->
        `Ty (Base.ty_app2 (module Type) env "Array" Ty.array)
      | Type.Id { Id.name = "select"; ns = Id.Term } ->
        `Term (Base.term_app2 (module Type) env "select" T.select)
      | Type.Id { Id.name = "store"; ns = Id.Term } ->
        `Term (Base.term_app3 (module Type) env "select" T.store)
      | _ -> `Not_found

  end

end
