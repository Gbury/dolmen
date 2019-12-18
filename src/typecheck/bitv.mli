

(** Smtlib bitvector builtins *)
module Smtlib : sig

  module type Ty = sig

    type t
    (** The type of types *)

    val bitv : int -> t
    (** Create a fixed size bitvector type. *)

  end

  module type T = sig

    type t
    (** The type of terms *)

    val mk_bitv : string -> t
    (** Create a bitvector litteral from a string representation.
        The string should only contain characters '0' or '1'. *)

    val bitv_concat : t -> t -> t
    (** Bitvector concatenation. *)

    val bitv_extract : int -> int -> t -> t
    (** Bitvector extraction, using the start and end position
        of the bitvector to extract. *)

    val bitv_repeat : int -> t -> t
    (** Repetition of a bitvector. *)

    val zero_extend : int -> t -> t
    (** Extend the given bitvector with the given numer of 0. *)

    val sign_extend : int -> t -> t
    (** Extend the given bitvector with its most significant bit
        repeated the given number of times. *)

    val rotate_right : int -> t -> t
    (** [rotate_right i x] means rotate bits of x to the right i times. *)

    val rotate_left : int -> t -> t
    (** [rotate_left i x] means rotate bits of x to the left i times. *)

    val bvnot : t -> t
    (** Bitwise negation. *)

    val bvand : t -> t -> t
    (** Bitwise conjunction. *)

    val bvor : t -> t -> t
    (** Bitwise disjunction. *)

    val bvnand : t -> t -> t
    (** [bvnand s t] abbreviates [bvnot (bvand s t)]. *)

    val bvnor : t -> t -> t
    (** [bvnor s t] abbreviates [bvnot (bvor s t)]. *)

    val bvxor : t -> t -> t
    (** [bvxor s t] abbreviates [bvor (bvand s (bvnot t)) (bvand (bvnot s) t)]. *)

    val bvxnor : t -> t -> t
    (** [bvxnor s t] abbreviates [bvor (bvand s t) (bvand (bvnot s) (bvnot t))]. *)

    val bvcomp : t -> t -> t
    (** Bitwise comparison. [bvcomp s t] equald [#b1] iff [s] and [t]
        are bitwise equal. *)


    val bvneg : t -> t
    (** Arithmetic complement on bitvectors.
        Supposing an input bitvector of size [m] representing
        an integer [k], the resulting term should represent
        the integer [2^m - k]. *)

    val bvadd : t -> t -> t
    (** Arithmetic addition on bitvectors, modulo the size of
        the bitvectors (overflows wrap around [2^m] where [m]
        is the size of the two input bitvectors). *)

    val bvsub : t -> t -> t
    (** Arithmetic substraction on bitvectors, modulo the size
        of the bitvectors (2's complement subtraction modulo).
        [bvsub s t] should be equal to [bvadd s (bvneg t)]. *)

    val bvmul : t -> t -> t
    (** Arithmetic multiplication on bitvectors, modulo the size
        of the bitvectors (see {!bvadd}). *)

    val bvudiv : t -> t -> t
    (** Arithmetic euclidian integer division on bitvectors. *)

    val bvurem : t -> t -> t
    (** Arithmetic euclidian integer remainder on bitvectors. *)

    val bvsdiv : t -> t -> t
    (** Arithmetic 2's complement signed division.
        (see smtlib's specification for more information). *)

    val bvsrem : t -> t -> t
    (** Arithmetic 2's coplement signed remainder (sign follows dividend).
        (see smtlib's specification for more information). *)

    val bvsmod : t -> t -> t
    (** Arithmetic 2's coplement signed remainder (sign follows divisor).
        (see smtlib's specification for more information). *)

    val bvshl : t -> t -> t
    (** Logical shift left. [bvshl t k] return the result of
        shifting [t] to the left [k] times. In other words,
        this should return the bitvector representing
        [t * 2^k] (since bitvectors represent integers using
        the least significatn bit in cell 0). *)

    val bvlshr : t -> t -> t
    (** Logical shift right. [bvlshr t k] return the result of
        shifting [t] to the right [k] times. In other words,
        this should return the bitvector representing
        [t / (2^k)]. *)

    val bvashr : t -> t -> t
    (** Arithmetic shift right, like logical shift right except that the most
        significant bits of the result always copy the most significant
        bit of the first argument*)

    val bvult : t -> t -> t
    (** Boolean arithmetic comparison (less than).
        [bvult s t] should return the [true] term iff [s < t]. *)

    val bvule : t -> t -> t
    (** Boolean arithmetic comparison (less or equal than). *)

    val bvugt : t -> t -> t
    (** Boolean arithmetic comparison (greater than). *)

    val bvuge : t -> t -> t
    (** Boolean arithmetic comparison (greater or equal than). *)

    val bvslt : t -> t -> t
    (** Boolean signed arithmetic comparison (less than).
        (See smtlib's specification for more information) *)

    val bvsle : t -> t -> t
    (** Boolean signed arithmetic comparison (less or equal than). *)

    val bvsgt : t -> t -> t
    (** Boolean signed arithmetic comparison (greater than). *)

    val bvsge : t -> t -> t
    (** Boolean signed arithmetic comparison (greater or equal than). *)

  end

  module Tff
      (Type : Tff_intf.S)
      (Ty : Ty with type t = Type.Ty.t)
      (T : T with type t = Type.T.t) : sig

    type Type.err +=
      | Invalid_bin_char of char
      | Invalid_hex_char of char

    val parse : Type.builtin_symbols
  end

end

