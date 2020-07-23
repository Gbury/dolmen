
(** {2 Languages Core builtins} *)

(** AE builtins *)
module Ae : sig

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Base with type t = Type.T.t) : sig

    val parse : Type.builtin_symbols

  end
end

(** TPTP builtins ($i, $o, etc..) *)
module Tptp : sig

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Base with type t = Type.T.t) : sig

    val parse : Dolmen.Tptp.version -> Type.builtin_symbols

  end
end


(** Smtlib builtin *)
module Smtlib2 : sig

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Smtlib_Base with type 'a t = 'a Type.Tag.t
                                          and type term := Type.T.t)
      (Ty : Dolmen.Intf.Ty.Smtlib_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Base with type t = Type.T.t
                                         and type cstr := Type.T.Cstr.t) : sig

    val parse : Dolmen.Smtlib2.version -> Type.builtin_symbols
  end
end

(** Zf builtins *)
module Zf : sig

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Zf_Base with type 'a t = 'a Type.Tag.t) : sig

    val parse : Type.builtin_symbols
  end
end
