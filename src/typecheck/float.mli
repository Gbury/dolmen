
(** Smtlib floating point builtins *)
module Smtlib2 : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Float with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Float with type t = Type.T.t) : sig

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Term.t Type.err
      | Invalid_hex_char : char -> Dolmen.Term.t Type.err
      | Bitvector_litteral_expected : Dolmen.Term.t Type.err
      | Bitvector_of_size_one_expected : int -> Dolmen.Term.t Type.err
      | To_fp_incorrect_args : Dolmen.Term.t Type.err

    val parse : Dolmen_smtlib2.version -> Type.builtin_symbols
  end

end
