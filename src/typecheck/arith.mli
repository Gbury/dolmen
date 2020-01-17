
(** Smtlib Integer and Real Arithmetic *)
module Smtlib : sig

  (** Standalone Integer arithmetic *)
  module Int : sig

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Int with type t := Type.T.t) : sig

      val parse : Type.builtin_symbols
    end

  end

  (** Standalone Integer arithmetic *)
  module Real : sig

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real with type t := Type.T.t) : sig

      val parse : Type.builtin_symbols
    end

  end

  (** Mixed Integer and Real arithmetic *)
  module Real_Int : sig

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real_Int with type t := Type.T.t
                                               and type ty := Type.Ty.t) : sig

    type Type.err +=
      | Expected_arith_type of Type.Ty.t
      (** Error raised when an arithmetic type was expected (i.e. either
          int or real), but another type was found. *)

    val parse : Type.builtin_symbols

    end

  end

end

(** TPTP Arithmetic *)
module Tptp : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Arith with type t := Type.T.t
                                        and type ty := Type.Ty.t) : sig

    type Type.err +=
      | Expected_arith_type of Type.Ty.t
      (** Error raised when an arithmetic type was expected (i.e. either
          int or real), but another type was found. *)
      | Cannot_apply_to of Type.Ty.t
      (** Raised when an arithmetic symbol is applied to an arithmetic
          type that cannot support the given operation (e.g. $quotient
          on integers). *)

    val parse : Type.builtin_symbols
  end

end
