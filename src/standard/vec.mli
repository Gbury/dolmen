
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Resieable arrays

    This is a minimalistic implementation of resizeable arrays. *)


(** {1 Type and array creation} *)

type 'a t
(** Abstract type of vectors of 'a *)

val make : int -> 'a -> 'a t
(** [make cap dummy] creates a new vector filled with [dummy]. The vector
    is initially empty but its underlying array has capacity [cap].
    [dummy] will stay alive as long as the vector *)

val create : unit -> 'a t
(** Create a fresh vector. *)

val copy : 'a t -> 'a t
(** Fresh copy *)


(** {1 Size manipulation} *)

val size : 'a t -> int
(** Size of the vector, aka number of elements in the vector. *)

val is_empty : 'a t -> bool
(** Is the vector empty ? *)

val is_full : 'a t -> bool
(** Is the capacity of the vector equal to the number of its elements? *)

val clear : 'a t -> unit
(** Set size to 0, doesn't free elements *)

val shrink : 'a t -> int -> unit
(** [shrink vec sz] resets size of [vec] to [sz].
    Assumes [sz >=0 && sz <= size vec] *)


(** {Get/Set operations} *)

val get : 'a t -> int -> 'a
(** get the element at the given index, or
    @raise Invalid_argument if the index is not valid *)

val last : 'a t -> 'a
(** get the last element, or
    @raise Invalid_argument if the vector is empty *)

val set : 'a t -> int -> 'a -> unit
(** set the element at the given index, either already set or the first
    free slot if [not (is_full vec)], or
    @raise Invalid_argument if the index is not valid *)

val push : 'a t -> 'a -> unit
(** Push element into the vector *)

val pop : 'a t -> 'a
(** Pop last element and return it.
    @raise Invalid_argument if the vector is empty *)

