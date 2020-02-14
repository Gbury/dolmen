
(** Smtlib array builtins *)
module Smtlib2 : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Array with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Array with type t := Type.T.t) : sig

    val parse : Dolmen_smtlib2.version -> Type.builtin_symbols
  end

end
