(** Ae floating point builtins *)
module Ae : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Float with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Float with type t := Type.T.t
                                          and type ty := Type.Ty.t) : sig

    val parse : Type.builtin_symbols
  end

end

(** Smtlib floating point builtins *)
module Smtlib2 : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Float with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Float with type t := Type.T.t
                                          and type ty := Type.Ty.t
                                          and type cst := Type.T.Const.t) : sig

    type _ Type.err +=
      | Invalid_bin_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_hex_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_dec_char : char -> Dolmen.Std.Term.t Type.err

    val parse : Dolmen.Smtlib2.version -> Type.builtin_symbols
  end

end
