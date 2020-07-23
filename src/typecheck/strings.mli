

(** Smtlib string builtins *)
module Smtlib2 : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_String with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_String with type t := Type.T.t) : sig

    type _ Type.err +=
      | Invalid_hexadecimal : string -> Dolmen.Std.Term.t Type.err
      | Invalid_string_char : char -> Dolmen.Std.Term.t Type.err
      | Invalid_escape_sequence : string * int -> Dolmen.Std.Term.t Type.err

    val parse : Dolmen.Smtlib2.version -> Type.builtin_symbols

  end

end
