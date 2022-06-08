
module Id = Dolmen.Std.Id

(* Ae arrays *)
(* ************************************************************************ *)

module Ae = struct

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Array with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Array with type t := Type.T.t) = struct

    type _ Type.err +=
      | Bad_farray_arity : Dolmen.Std.Term.t Type.err

    let parse env s =
      match s with
      | Type.Id { name = Simple "farray"; ns = Sort } ->
        `Ty (
          fun ast args ->
            match args with
            | [ty] ->
              Ty.array Ty.int (Type.parse_ty env ty)
            | [ity; vty] ->
              Ty.array (Type.parse_ty env ity) (Type.parse_ty env vty)
            | _ -> Type._error env (Ast ast) Bad_farray_arity
        )
      | Type.Builtin Array_get ->
        `Term (Base.term_app2 (module Type) env s T.select)
      | Type.Builtin Array_set ->
        `Term (Base.term_app3 (module Type) env s T.store)
      | _ -> `Not_found
  end

end


(* Smtlib arrays *)
(* ************************************************************************ *)

module Smtlib2 = struct

  (* Restrictions imposed by some theories on the kinds of arrays that
     are allowed to occur. This tecnically brings some cross-theory
     dependency, but since the restrictions are simple enough, we can
     get by with simple syntactic checks on the sort declarations.
     The restrictions are as follows:
     - AUFLIA, QF_AUFLIA : int -> int
     - AUFLIRA : int -> real & int -> (int -> real)
     - QF_ABV, QF_AUFBV : bitvec _ -> bitvec _
  *)
  type arrays =
    | All
    | Only_int_int
    | Only_ints_real
    | Only_bitvec

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Array with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Array with type t := Type.T.t) = struct

    type _ Type.err +=
      | Forbidden : string -> Dolmen.Std.Term.t Type.err

    let msg = function
      | All -> assert false
      | Only_int_int ->
        "Only array types of the form (Array Int Int) are allowed by the logic"
      | Only_ints_real ->
        "Only array types of the form (Array Int Int) or (Array Int (Array (Int Real))) \
         are allowed by the logic"
      | Only_bitvec ->
        "Only array types of the form (Array (_ BitVec i) (_ BitVec j)) for some i, j \
         are allowed by the logic"

    let mk_array_ty env arrays ast src dst =
      let error () = Type._error env (Ast ast) (Forbidden (msg arrays)) in
      begin match arrays, Ty.view src, Ty.view dst with
        | All, _, _ -> ()
        (* AUFLIA, QF_AUFLIA restrictions *)
        | Only_int_int, `Int, `Int -> ()
        | Only_int_int, _, _ -> error ()
        (* AUFLIRA restriction *)
        | Only_ints_real, `Int, `Real -> ()
        | Only_ints_real, `Int, `Array (src', dst') ->
          begin match Ty.view src', Ty.view dst' with
            | `Int, `Real -> ()
            | _, _ -> error ()
          end
        | Only_ints_real, _, _ -> error ()
        (* QF_ABV, QF_AUFBV restrictions *)
        | Only_bitvec, `Bitv _, `Bitv _ -> ()
        | Only_bitvec, _, _ -> error ()
      end;
      Ty.array src dst

    let parse ~arrays _version env s =
      match s with
      | Type.Id { name = Simple "Array"; ns = Sort } ->
        `Ty (Base.ty_app2_ast (module Type) env s (mk_array_ty env arrays))
      | Type.Id { name = Simple "select"; ns = Term } ->
        `Term (Base.term_app2 (module Type) env s T.select)
      | Type.Id { name = Simple "store"; ns = Term } ->
        `Term (Base.term_app3 (module Type) env s T.store)
      | _ -> `Not_found

  end

end
