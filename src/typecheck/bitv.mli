


(** Alt-Ergo bitvector builtins *)
module Ae : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Bitv with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Bitv with type t := Type.T.t) : sig

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Std.Term.t Type.err
      (** Error raised when a character that isn't '0' or '1' is used inside a
          literal bitvector in binary form. *)
      | Expected_nat_lit : Dolmen.Std.Term.t -> Dolmen.Std.Term.t Type.err
      (** Error raised when a function that expects a literal natural number
          as an argument receives something else. *)
    (** Additional errors specific to Alt-Ergo's bitvectors' typing. *)

    val parse : Type.builtin_symbols

  end

end

(** Smtlib bitvector builtins *)
module Smtlib2 : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Bitv with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Bitv with type t := Type.T.t) : sig

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_hex_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_dec_char : char -> Dolmen.Std.Term.t Type.err

    val parse : Dolmen.Smtlib2.version -> Type.builtin_symbols

  end

end
