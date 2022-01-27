
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Type definitions} *)
(** ************************************************************************ *)

type 'a witness = ..
type any_witness = Any : _ witness -> any_witness [@@unboxed]

module type S = sig

  type t

  type _ witness += Val : t witness

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

end

type 'a ops = private (module S with type t = 'a)

type t = private Value : 'a witness * 'a ops * 'a -> t



(* Creating/extracting values *)
(* ************************************************************************* *)

val mk : ops:'a ops -> 'a -> t

val extract : ops:'a ops -> t -> 'a option

val extract_exn : ops:'a ops -> t -> 'a


(** {2 Custom operations} *)
(** ************************************************************************ *)

val ops :
  compare:('a -> 'a -> int) ->
  print:(Format.formatter -> 'a -> unit) ->
  'a ops

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

