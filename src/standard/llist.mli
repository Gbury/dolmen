
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 List Length} *)
(*  ************************************************************************* *)

type z = Z
(** The type for the length "0" *)

type 'a succ = S
(** The type for the length "succesor of 'a". *)

type one = z
type two = z succ
type three = z succ succ
(** Useful type abbreviations *)


(** {2 Llists} *)
(*  ************************************************************************* *)

type (_, _) t =
  | []    : (z, 'a) t
  | (::)  : 'a * ('n, 'a) t -> ('n succ, 'a) t (**)
(** The type of llists, i.e. lists with their length encoded in the type. *)

val hash : ('a -> int) -> (_, 'a) t -> int
(** Hash a llist. *)

val compare : ('a -> 'a -> int) -> (_, 'a) t -> (_, 'a) t -> int
(** Comparison function on llists. *)

val print :
  sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> (_, 'a) t -> unit
(** Printing function for llists. *)


(** {2 Inspection} *)
(*  ************************************************************************* *)

val head : ('n succ, 'a) t -> 'a
(** head of a non-empty list *)

val tail : ('n succ, 'a) t -> ('n, 'a) t
(** tail of a non-empty list *)

val length : _ t -> int
(** Length of a list. *)

val to_seq : (_, 'a) t -> 'a Seq.t
(** Get an iterator over the list ignoring the length. *)

