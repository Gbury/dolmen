
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Type definitions} *)
(** ************************************************************************ *)

type t
type 'a ops
type any_ops = Ops : _ ops -> any_ops

exception Extraction_failed of t * any_ops

(* Creating/extracting values *)
(* ************************************************************************* *)

val dummy : t

val mk : ops:'a ops -> 'a -> t

val extract : ops:'a ops -> t -> 'a option

val extract_exn : ops:'a ops -> t -> 'a

val abstract_cst : Dolmen.Std.Expr.Term.Const.t -> t


(** {2 Custom operations} *)
(** ************************************************************************ *)

val ops :
  ?abstract:(Dolmen.Std.Expr.Term.Const.t -> 'a) ->
  compare:('a -> 'a -> int) ->
  print:(Format.formatter -> 'a -> unit) ->
  unit -> 'a ops

val print : Format.formatter -> t -> unit

val compare : t -> t -> int


(** {2 Sets and Maps} *)
(** ************************************************************************ *)

module Set : Set.S with type elt = t
module Map : Map.S with type key = t


