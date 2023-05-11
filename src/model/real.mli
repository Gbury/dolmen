
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module A : sig
   (** Algebraic numbers *)
  type t

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

(** {2 Value definition} *)
(** ************************************************************************ *)

type t = A.t

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


