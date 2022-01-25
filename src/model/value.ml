
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions *)
(* ************************************************************************* *)

type 'a witness = ..

type 'a ops = {
  compare : 'a -> t -> int;
  print : Format.formatter -> 'a -> unit;
}

and 'a spec = {
  ops : 'a ops;
  witness : 'a witness;
}

and t = Value : 'a spec * 'a -> t


(* Wrapper around custom operations *)
(* ************************************************************************* *)

let compare t t' =
  let Value ({ ops; witness= _; }, x) = t in
  ops.compare x t'

let print fmt t =
  let Value ({ ops; witness = _; }, x) = t in
  ops.print fmt x


(* Wrapper around custom operations *)
(* ************************************************************************* *)




