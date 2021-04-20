
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

val get : map -> 'a t -> 'a option
(** Get the value associated to a tag. *)

val get_list : map -> 'a list t -> 'a list
(** Get all the values associated with a tag list, returning
    the empty list by default if the tag is not present. *)

val get_last : map -> 'a list t -> 'a option
(** Return the last value associated to a list tag (i.e. the head of the
    list returned by {get_list} if it exists). *)

val set : map -> 'a t -> 'a -> map
(** Set the value bound to a tag. *)

val set_opt : map -> 'a t -> 'a option -> map
(** Convenient shorthand for an optional set. *)

val add : map -> 'a list t -> 'a -> map
(** Add a value to a list tag in a map. The new value is enqueued
    at the head of the list of values bound. *)

val add_opt : map -> 'a list t -> 'a option -> map
(** Optionally add a value to a list tag in a map. The new value is enqueued
    at the head of the list of values bound. *)

val add_list : map -> 'a list t -> 'a list -> map
(** Add a list of values to a list tag in a map. The new values are enqueued
    at the head of the list of values bound, however it is not guaranteed that
    the first value of the given list is the new head of the list of bound values. *)

val unset : map -> _ t -> map
(** Remove any binding to the given key in the map. *)

