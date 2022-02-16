
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions *)
(* ************************************************************************* *)

type 'a witness = ..
type any_witness = Any : _ witness -> any_witness [@@unboxed]

module type S = sig

  type t

  type _ witness += Val : t witness

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

end

type 'a ops = (module S with type t = 'a)

type t = Value : 'a witness * 'a ops * 'a -> t


(* Create a custom ops *)
(* ************************************************************************* *)

let ops (type a)
    ~(compare : a -> a -> int)
    ~(print : Format.formatter -> a -> unit)
  : a ops =
  let module M = struct
    type t = a
    type _ witness += Val : t witness
    let print = print
    let compare = compare
  end in
  (module M : S with type t = a)


(* Creating/extracting values *)
(* ************************************************************************* *)

let[@inline] mk (type a) ~(ops: a ops) (x : a) =
  let (module V) = ops in
  Value (V.Val, ops, x)

let[@inline] extract (type a) ~(ops: a ops) (t : t) : a option =
  let (module V) = ops in
  match t with
  | Value (V.Val, _, x) -> Some x
  | _ -> None

let[@inline] extract_exn (type a) ~(ops: a ops) (t : t) : a =
  let (module V) = ops in
  match t with
  | Value (V.Val, _, x) -> x
  | _ -> assert false


(* Wrapper around custom operations *)
(* ************************************************************************* *)

let print fmt t =
  let Value (_, (module V), x) = t in
  V.print fmt x

let compare t t' =
  let Value (w, (module V), x) = t in
  match t' with
  | Value (V.Val, _, y) -> (V.compare x y : int)
  | Value (w', _, _) -> Stdlib.compare (Any w) (Any w')

(* Sets/Maps *)
(* ************************************************************************* *)

module Aux = struct
  type aux = t
  type t = aux
  let compare = compare
end

module Set = Set.Make(Aux)
module Map = Map.Make(Aux)

