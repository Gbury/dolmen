
(** Smtlib Integer and Real Arithmetic *)
module Smtlib2 : sig

  type arith =
    | Regular
    | Linear of [ `Large | `Strict ]
    | Difference of [ `IDL | `RDL | `UFIDL ] (**)
  (** The different type of arithmetic restrictions, see the comment in
      arith.ml for more information. *)

  (** Standalone Integer arithmetic *)
  module Int : sig

    type _ Type.warn +=
      | Restriction : string -> Dolmen.Std.Term.t Type.warn
      (** Warning for expressions which tecnically do not respect the strict
          spec but respect the large spec. *)
    (** Arithmetic type-checking warnings *)

    type _ Type.err +=
      | Forbidden : string -> Dolmen.Std.Term.t Type.err
      (** Error for expressions which do not respect the spec. *)
    (** Arithmetic type-checking errors *)

    val parse : arith:arith -> Dolmen.Smtlib2.version -> Type.builtin_symbols
    (** Parsing function for type-checking *)
  end


  (** Standalone Integer arithmetic *)
  module Real : sig

    type _ Type.warn +=
      | Restriction : string -> Dolmen.Std.Term.t Type.warn
      (** Warning for expressions which tecnically do not respect the strict
          spec but respect the large spec. *)
    (** Arithmetic type-checking warnings *)

    type _ Type.err +=
      | Forbidden : string -> Dolmen.Std.Term.t Type.err
      (** Error for expressions which do not respect the spec. *)
    (** Arithmetic type-checking errors *)

    val parse : arith:arith -> Dolmen.Smtlib2.version -> Type.builtin_symbols
    (** Parsing function for type-checking *)

  end

  (** Mixed Integer and Real arithmetic *)
  module Real_Int : sig

    type _ Type.warn +=
      | Restriction : string -> Dolmen.Std.Term.t Type.warn
      (** Warning for expressions which tecnically do not respect the strict
          spec but respect the large spec. *)
    (** Arithmetic type-checking warnings *)

    type _ Type.err +=
      | Forbidden : string -> Dolmen.Std.Term.t Type.err
      (** Error for expressions which do not respect the spec. *)
      | Expected_arith_type : Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Term.t Type.err
      (** Error raised when an arithmetic type was expected (i.e. either
          int or real), but another type was found. *)
    (** Additional errors specific to arithmetic typing. *)

    val parse : arith:arith -> Dolmen.Smtlib2.version -> Type.builtin_symbols
    (** Parsing function for type-checking *)

  end

end

(** TPTP Arithmetic *)
module Tptp : sig

  type _ Type.err +=
    | Expected_arith_type : Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Term.t Type.err
    (** Error raised when an arithmetic type was expected (i.e. either
        int or real), but another type was found. *)
    | Cannot_apply_to : Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Term.t Type.err
    (** Raised when an arithmetic symbol is applied to an arithmetic
        type that cannot support the given operation (e.g. $quotient
        on integers). *)
  (** Additional errors specific to arithmetic typing. *)

  val parse : Dolmen.Tptp.version -> Type.builtin_symbols

end
