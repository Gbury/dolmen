
(** Ae array builtins *)
module Ae : sig

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Array with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Array with type t := Type.T.t) : sig

    type _ Type.err +=
      | Bad_farray_arity : Dolmen.Std.Term.t Type.err
      (** Raised when an array is parametrized
          with other than one or two parameters. *)
    (** Errors for array type-checking. *)

    val parse : Type.builtin_symbols
  end

end


(** Smtlib array builtins *)
module Smtlib2 : sig

  type config =
    | All
    | Only_int_int
    | Only_ints_real
    | Only_bitvec (**)
  (** The difference type of array restrictions that can be imposed by
      logics. *)

  val print_config : Format.formatter -> config -> unit

  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Smtlib_Array with type t := Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Array with type t := Type.T.t
                                          and type ty := Type.Ty.t) : sig

    type _ Type.err +=
      | Forbidden : string -> Dolmen.Std.Term.t Type.err
      (** Raised when a restriction on the sort of arrays is breached. *)
    (** Errors for array type-checking. *)

    type _ Type.warn +=
      | Extension : Dolmen.Std.Id.t -> Dolmen.Std.Term.t Type.warn
      (** Raised when an id belonging to an extension of the array theory is used
          (typically `const`). *)
    (** Warnings for array type-checking. *)

    val parse : config:config -> Dolmen.Smtlib2.version -> Type.builtin_symbols
  end

end
