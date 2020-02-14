
(** Smtlib array builtins *)
module Smtlib : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Array with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Array with type t := Type.T.t) : sig

    val parse : Type.builtin_symbols
  end

end
