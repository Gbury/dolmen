

(** Smtlib bitvector builtins *)
module Smtlib : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Bitv with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Bitv with type t = Type.T.t) : sig

    type Type.err +=
      | Invalid_bin_char of char
      | Invalid_hex_char of char

    val parse : Type.builtin_symbols
  end

end

