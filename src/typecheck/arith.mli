
(** Smtlib Integer and Real Arithmetic *)
module Smtlib : sig

  (** Standalone Integer arithmetic *)
  module Int : sig

    module type Ty = sig
      type t
      (** The type of types *)
      val int : t
      (** The type for integer expressions. *)
    end

    module type T = sig
      type t
      (** The type of terms *)
      val int : string -> t
      (** Build an integer constant. The integer is passed
          as a string, and not an [int], to avoid overflow caused
          by the limited precision of native intgers. *)
      val neg : t -> t
      (** Arithmetic negation. *)
      val add : t -> t -> t
      (** Arithmetic addition. *)
      val sub : t -> t -> t
      (** Arithmetic substraction *)
      val mul : t -> t -> t
      (** Arithmetic multiplication *)
      val div : t -> t -> t
      (** Integer division. See Smtlib theory for a full description. *)
      val modulo : t -> t -> t
      (** Integer remainder See Smtlib theory for a full description. *)
      val abs : t -> t
      (** Arithmetic absolute value. *)
      val lt : t -> t -> t
      (** Arithmetic "less than" comparison. *)
      val le : t -> t -> t
      (** Arithmetic "less or equal" comparison. *)
      val gt : t -> t -> t
      (** Arithmetic "greater than" comparison. *)
      val ge : t -> t -> t
      (** Arithmetic "greater or equal" comparison. *)
      val divisible : string -> t -> t
      (** Arithmetic divisibility predicate. Indexed over
          constant integers (represented as strings, see {!int}). *)
    end

    module Tff
        (Type : Tff_intf.S)
        (Ty : Ty with type t = Type.Ty.t)
        (T : T with type t = Type.T.t) : sig

      val parse : Type.builtin_symbols
    end

  end

  (** Standalone Integer arithmetic *)
  module Real : sig

    module type Ty = sig
      type t
      (** The type of types *)
      val real : t
      (** The type for integer expressions. *)
    end

    module type T = sig
      type t
      (** The type of terms *)
      val real : string -> t
      (** Build a real constant. The string should respect
          smtlib's syntax for INTEGER or DECIMAL. *)
      val neg : t -> t
      (** Arithmetic negation. *)
      val add : t -> t -> t
      (** Arithmetic addition. *)
      val sub : t -> t -> t
      (** Arithmetic substraction *)
      val mul : t -> t -> t
      (** Arithmetic multiplication *)
      val div : t -> t -> t
      (** Real division. *)
      val lt : t -> t -> t
      (** Arithmetic "less than" comparison. *)
      val le : t -> t -> t
      (** Arithmetic "less or equal" comparison. *)
      val gt : t -> t -> t
      (** Arithmetic "greater than" comparison. *)
      val ge : t -> t -> t
      (** Arithmetic "greater or equal" comparison. *)
    end

    module Tff
        (Type : Tff_intf.S)
        (Ty : Ty with type t = Type.Ty.t)
        (T : T with type t = Type.T.t) : sig

      val parse : Type.builtin_symbols
    end

  end

  (** Mixed Integer and Real arithmetic *)
  module Real_Int : sig

    module type Ty = sig
      type t
      (** The type of types *)
      val int : t
      (** The type for integer expressions. *)
      val real : t
      (** The type for integer expressions. *)
      val equal : t -> t -> bool
      (** Equality function on types. *)
    end

    module type T = sig
      type t
      (** The type of terms. *)
      type ty
      (** The type of types. *)
      val ty : t -> ty
      (** Get the type of a term. *)
      val int : string -> t
      (** Create a term from a constant integer. *)
      val real : string -> t
      (** Create a term from a constant real. *)

      (** Integer operations on terms *)
      module Int : sig
        val neg : t -> t
        (** Arithmetic negation. *)
        val add : t -> t -> t
        (** Arithmetic addition. *)
        val sub : t -> t -> t
        (** Arithmetic substraction *)
        val mul : t -> t -> t
        (** Arithmetic multiplication *)
        val div : t -> t -> t
        (** Integer division. See Smtlib theory for a full description. *)
        val modulo : t -> t -> t
        (** Integer remainder See Smtlib theory for a full description. *)
        val abs : t -> t
        (** Arithmetic absolute value. *)
        val lt : t -> t -> t
        (** Arithmetic "less than" comparison. *)
        val le : t -> t -> t
        (** Arithmetic "less or equal" comparison. *)
        val gt : t -> t -> t
        (** Arithmetic "greater than" comparison. *)
        val ge : t -> t -> t
        (** Arithmetic "greater or equal" comparison. *)
        val divisible : string -> t -> t
        (** Arithmetic divisibility predicate. Indexed over
            constant integers (represented as strings, see {!int}). *)
        val to_real : t -> t
        (** Conversion from an integer term to a real term. *)
      end

      (** Real operations on terms *)
      module Real : sig
        val neg : t -> t
        (** Arithmetic negation. *)
        val add : t -> t -> t
        (** Arithmetic addition. *)
        val sub : t -> t -> t
        (** Arithmetic substraction *)
        val mul : t -> t -> t
        (** Arithmetic multiplication *)
        val div : t -> t -> t
        (** Real division. *)
        val lt : t -> t -> t
        (** Arithmetic "less than" comparison. *)
        val le : t -> t -> t
        (** Arithmetic "less or equal" comparison. *)
        val gt : t -> t -> t
        (** Arithmetic "greater than" comparison. *)
        val ge : t -> t -> t
        (** Arithmetic "greater or equal" comparison. *)
        val is_int : t -> t
        (** Arithmetic predicate, true on reals that are also integers. *)
        val to_int : t -> t
        (** Partial function from real to integers. Only has defined semantics
            when {!is_int} is true. *)
      end

    end

    module Tff
        (Type : Tff_intf.S)
        (Ty : Ty with type t = Type.Ty.t)
        (T : T with type t = Type.T.t and type ty := Type.Ty.t) : sig

    type Type.err +=
      | Expected_arith_type of Ty.t
      (** Error raised when an arithmetic type was expected (i.e. either
          int or real), but another type was found. *)

      val parse : Type.builtin_symbols
    end

  end

end
