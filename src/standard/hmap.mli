
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Heterogeneous Maps} *)

type 'a injection
(** An accessor for values of type 'a in any map. Values put
    in the map using a key can only be retrieved using this
    very same key. *)

val create_inj : unit -> 'a injection
(** Return a value that works for a given type of values.  This function is
    normally called once for each type of value. Several keys may be
    created for the same type, but a value set with a given setter can only be
    retrieved with the matching getter. The same key can be reused
    across multiple maps (although not in a thread-safe way). *)

module type S = sig
  type key

  type t
  (** A map containing values of different types, indexed by {!key}. *)

  val empty : t
  (** Empty map. *)

  val get : inj:'a injection -> key -> t -> 'a option
  (** Get the value corresponding to this key, if it exists and
      belongs to the same key. *)

  val add : inj:'a injection -> key -> 'a -> t -> t
  (** Bind the key to the value, using [inj]. *)

  val update : inj:'a injection -> key -> ('a option -> 'a option) -> t -> t
  (** [update ~inj k f m] updates the value associated with [k] in [m] according
      to [f (get ~inj k m)]. If the result is [None], the binding associated
      with [k] is removed.

      @since 0.9 *)

  val find : inj:'a injection -> key -> t -> 'a
  (** Find the value for the given key, which must be of the right type.
      @raise Not_found if either the key is not found, or if its value
        doesn't belong to the right type. *)

  val cardinal : t -> int
  (** Number of bindings. *)

  val remove : key -> t -> t
  (** Remove the binding for this key. *)

  val mem : inj:_ injection-> key -> t -> bool
  (** Is the given key in the map, with the right type? *)

  val iter_keys : f:(key -> unit) -> t -> unit
  (** Iterate on the keys of this map. *)

  val fold_keys : f:('a -> key -> 'a) -> x:'a -> t -> 'a
  (** Fold over the keys. *)

  (** {2 Iterators} *)

  type 'a iter = ('a -> unit) -> unit

  val keys_iter : t -> key iter
  (** All the keys. *)

  val bindings_of : inj:'a injection -> t -> (key * 'a) iter
  (** All the bindings that come from the corresponding injection. *)

  type value =
    | Value : ('a injection -> 'a option) -> value

  val bindings : t -> (key * value) iter
  (** Iterate on all bindings. *)
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORD) : S with type key = X.t
