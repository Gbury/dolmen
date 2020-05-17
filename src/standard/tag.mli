
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** Tags *)

(** {2 Type definitions} *)

type map
(** The type of immutable maps from tags to values. *)

type 'a t
(** A tag containing values of type ['a]. *)

val equal : _ t -> _ t -> bool
(** Are two tag keys equal ? *)


(** {2 Creating and accessing tags} *)

val empty : map
(** The empty map. *)

val create : unit -> 'a t
(** Create a new tag. *)

val get : map -> 'a t -> 'a list
(** Get the list of values associated to a tag
    (can be the empty list). *)

val last : map -> 'a t -> 'a option
(** Return the last value associated to a tag (i.e. the head of the
    list returned by {get} if it exists). *)

val add : map -> 'a t -> 'a -> map
(** Add a value to a tag in a map. If some values were previously bound,
    this adds the new value to the head of the value list. *)

val replace : map -> 'a t -> 'a list -> map
(** Replace the set of values associated to a tag in a map. *)

