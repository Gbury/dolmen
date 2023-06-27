
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Value definition} *)
(** ************************************************************************ *)

type t

val ops : t Value.ops
(** ops for real values. *)

val mk : t -> Value.t
(** real value creation. *)

val get : Value.t -> t
(** Get a rational value. *)


(** {2 Corner cases & builtins} *)
(** ************************************************************************ *)

val builtins : Env.builtins
(** builtins for reals *)


(** {2 Algebraic number manipulation} *)
(** ************************************************************************ *)

(** Polynomials *)
module Poly = Flint.FMPZ_poly

(** Algebraic numbers *)
module A : sig

  type nonrec t = t
  (** The type of algebraic numbers *)

  exception Complex_roots of {
      poly : Poly.t;
    }
  (** Exception raised when trying to create an algebraic number
      from a polynomial that has complex roots. *)

  exception No_ordered_root of {
      order : int;
      poly : Poly.t;
      num_roots : int;
    }
  (** Exception raised when trying to create an algebraic number
      from the [order]-th root (first root = 0) of [poly], but [poly]
      only has [num_roots] (with [num_roots <= order]). *)

  exception Bad_root_enclosure of {
      poly : Poly.t;
      min : Q.t;
      max : Q.t;
      roots : t list;
    }
  (** Exception raised when trying to create an algebraic number
      from the root of [poly] that is within [min] and [max],
      but there was not exactly 1 root in that interval. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  val zero : t
  val one : t
  val half : t
  val minus_one : t
  val two : t

  val sign : t -> int
  val ge : t -> t -> bool
  val gt : t -> t -> bool
  val le : t -> t -> bool
  val lt : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
  val is_integer : t -> bool
  val is_real : t -> bool
  val to_z : t -> Z.t
  val to_int : t -> int
  val to_q : t -> Q.t option
  val inf : t
  val minus_inf : t
  val is_unsigned_integer : int -> t -> bool
  val of_q : Q.t -> t
  val of_z : Z.t -> t
  val of_int : int -> t
  val of_bigint : Z.t -> t
  val floor : t -> t
  val ceil : t -> t
  val truncate : t -> t
  val neg : t -> t
  val inv : t -> t
  val abs : t -> t
  val div : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( ~+ ) : 'a -> 'a
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val div_e : t -> t -> t
  val div_t : t -> t -> t
  val div_f : t -> t -> t
  val mod_e : t -> t -> t
  val mod_t : t -> t -> t
  val mod_f : t -> t -> t
  val is_zero : t -> bool
  val round : t -> t
  val none_zero : t -> t option
  val is_not_zero : t -> bool
  val pow : t -> int -> t
  val positive_root : t -> int -> t
  val positive_pow : t -> Q.t -> t
end


