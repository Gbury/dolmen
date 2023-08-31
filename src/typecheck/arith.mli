
(** AE Integer Arithmetic *)
module Ae: sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Arith with type t := Type.T.t
                                        and type ty := Type.Ty.t) : sig

    type _ Type.err +=
      | Expected_arith_type : Type.Ty.t -> Dolmen.Std.Term.t Type.err
      (** Error raised when an arithmetic type was expected (i.e. either
          int or real), but another type was found. *)
    (** Additional errors specific to arithmetic typing. *)

    val parse : Type.builtin_symbols
  end

end

(** Smtlib Integer and Real Arithmetic *)
module Smtlib2 : sig

  type config =
    | Regular
    | Linear of [ `Large | `Strict ]
    | Difference of [ `IDL | `RDL | `UFIDL ] (**)
  (** The different type of arithmetic restrictions, see the comment in
      arith.ml for more information. *)

  val print_config : Format.formatter -> config -> unit

  (** Standalone Integer arithmetic *)
  module Int : sig

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Int with type t := Type.T.t
                                          and type cst := Type.T.Const.t) : sig

      type _ Type.warn +=
        | Restriction : config * string -> Dolmen.Std.Term.t Type.warn
        (** Warning for expressions which tecnically do not respect the strict
            spec but respect the large spec. *)
      (** Arithmetic type-checking warnings *)

      type _ Type.err +=
        | Forbidden : config * string -> Dolmen.Std.Term.t Type.err
        (** Error for expressions which do not respect the spec. *)
      (** Arithmetic type-checking errors *)

      val parse : config:config -> Dolmen.Smtlib2.version -> Type.builtin_symbols
      (** Parsing function for type-checking *)
    end

  end

  (** Standalone Integer arithmetic *)
  module Real : sig

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real with type t := Type.T.t
                                           and type cst := Type.T.Const.t) : sig

      type _ Type.warn +=
        | Restriction : config * string -> Dolmen.Std.Term.t Type.warn
        (** Warning for expressions which tecnically do not respect the strict
            spec but respect the large spec. *)
      (** Arithmetic type-checking warnings *)

      type _ Type.err +=
        | Forbidden : config * string -> Dolmen.Std.Term.t Type.err
        (** Error for expressions which do not respect the spec. *)
      (** Arithmetic type-checking errors *)

      val parse : config:config -> Dolmen.Smtlib2.version -> Type.builtin_symbols
      (** Parsing function for type-checking *)
    end

  end

  (** Mixed Integer and Real arithmetic *)
  module Real_Int : sig

    module Tff
        (Type : Tff_intf.S)
        (Ty : Dolmen.Intf.Ty.Smtlib_Real_Int with type t := Type.Ty.t)
        (T : Dolmen.Intf.Term.Smtlib_Real_Int with type t := Type.T.t
                                               and type ty := Type.Ty.t
                                               and type Int.cst := Type.T.Const.t
                                               and type Real.cst := Type.T.Const.t) : sig

      type _ Type.warn +=
        | Restriction : config * string -> Dolmen.Std.Term.t Type.warn
        (** Warning for expressions which tecnically do not respect the strict
            spec but respect the large spec. *)
      (** Arithmetic type-checking warnings *)

      type _ Type.err +=
        | Forbidden : config * string -> Dolmen.Std.Term.t Type.err
        (** Error for expressions which do not respect the spec. *)
        | Expected_arith_type : Type.Ty.t -> Dolmen.Std.Term.t Type.err
        (** Error raised when an arithmetic type was expected (i.e. either
            int or real), but another type was found. *)
      (** Additional errors specific to arithmetic typing. *)

      val parse : config:config -> Dolmen.Smtlib2.version -> Type.builtin_symbols
      (** Parsing function for type-checking *)

    end

  end

end

(** TPTP Arithmetic *)
module Tptp : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Tff_Arith with type t := Type.T.t
                                        and type ty := Type.Ty.t) : sig

    type _ Type.err +=
      | Expected_arith_type : Type.Ty.t -> Dolmen.Std.Term.t Type.err
      (** Error raised when an arithmetic type was expected (i.e. either
          int or real), but another type was found. *)
      | Cannot_apply_to : Type.Ty.t -> Dolmen.Std.Term.t Type.err
      (** Raised when an arithmetic symbol is applied to an arithmetic
          type that cannot support the given operation (e.g. $quotient
          on integers). *)
    (** Additional errors specific to arithmetic typing. *)

    val parse : Dolmen.Tptp.version -> Type.builtin_symbols
  end

end

(** Zf Arithmetic *)
module Zf : sig

  module Thf
      (Type : Thf_intf.S)
      (Ty : Dolmen.Intf.Ty.Zf_Arith with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Zf_Arith with type t := Type.T.t) : sig

    val parse : Type.builtin_symbols
  end

end
