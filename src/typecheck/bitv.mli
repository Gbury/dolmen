

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

    val bvnot : t -> t
    (** Bitwise negation. *)
    val bvand : t -> t -> t
    (** Bitwise conjunction. *)
    val bvor : t -> t -> t
    (** Bitwise disjunction. *)

    val bvneg : t -> t
    (** Arithmetic complement on bitvectors.
        Supposing an input bitvector of size [m] representing
        an integer [k], the resulting term should represent
        the integer [2^m - k]. *)
    val bvadd : t -> t -> t
    (** Arithmetic addition on bitvectors, modulo the size of
        the bitvectors (overflows wrap around [2^m] where [m]
        is the size of the two input bitvectors). *)
    val bvmul : t -> t -> t
    (** Arithmetic multiplication on bitvectors, modulo the size
        of the bitvectors (see {!bvadd}). *)
    val bvudiv : t -> t -> t
    (** Arithmetic euclidian integer division on bitvectors. *)
    val bvurem : t -> t -> t
    (** Arithmetic euclidian integer remainder on bitvectors. *)

    val bvshl : t -> t -> t
    (** Logical right shift. [bvshl t k] return the result of
        shifting [t] to the right [k] times. In other words,
        this should return the bitvector representing
        [t * 2^k] (since bitvectors represent integers using
        the least significatn bit in cell 0). *)
    val bvlshr : t -> t -> t
    (** Logical left shift. [bvlshr t k] return the result of
        shifting [t] to the left [k] times. In other words,
        this should return the bitvector representing
        [t / (2^k)]. *)
    val bvult : t -> t -> t
    (** Boolean arithmetic comparison. [bvult s t] should return
        the [true] term iff [s < t]. *)
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

