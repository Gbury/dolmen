

(** Smtlib bitvector builtins *)
module Smtlib2 : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Bitv with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Bitv with type t = Type.T.t) : sig

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Term.t Type.err
      | Invalid_hex_char : char -> Dolmen.Term.t Type.err
      | Invalid_dec_char : char -> Dolmen.Term.t Type.err

    val parse : Dolmen_smtlib2.version -> Type.builtin_symbols

    val parse_binary : Type.env -> Dolmen_std.Term.t -> string -> Type.res
    (** parse binary constant into bitvector constants *)

    val parse_hexa   : Type.env -> Dolmen_std.Term.t -> string -> Type.res
    (** parse hexa constant into bitvector constants *)
  end

end
