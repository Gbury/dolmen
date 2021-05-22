
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = sig

  type key
  type 'a t

  val empty : _ t

  val find_exn : key -> 'a t -> 'a
  (** Exception-raising find function.
      @raise Not_found *)

  val find_opt : key -> 'a t -> 'a option
  (** Option-returning find function. *)

  val add : key -> 'a -> 'a t -> 'a t
  (** Add a new binding, shadowing any earlier bdingin to the same key. *)

  val find_add : key -> ('a option -> 'a) -> 'a t -> 'a t
  (** Update the value bound to a key. *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** Iter on the map. *)

  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  (** Fold on the map. *)

end
