
(** {2 Logics}

    Some languages define a notion of logics to distinguish different
    fragments at type-checking and during solving. This file is here to
    help with handling such logics.
*)

(** {2 Smtlib logics} *)
module Smtlib2 : sig

  type theory = [
    | `Core
    | `Arrays
    | `Bitvectors
    | `Floats
    | `String
    | `Ints
    | `Reals
    | `Reals_Ints
  ]
  (** Smtlib theories. *)

  type features = {
    free_sorts      : bool;
    free_functions  : bool;
    datatypes       : bool;
    quantifiers     : bool;
    arithmetic      : Arith.Smtlib2.config;
    arrays          : Arrays.Smtlib2.config;
  }
  (** Smtlib features. *)

  type t = {
    theories      : theory list;
    features      : features;
  }
  (** Structured representation of an smtlib logic. *)

  val print : Format.formatter -> t -> unit
  val print_theory : Format.formatter -> theory -> unit
  val print_theories : Format.formatter -> theory list -> unit
  val print_features : Format.formatter -> features -> unit
  (** Printing functions *)

  val parse : string -> t option
  (** Parses an smtlib logic string and returns its structured version. *)

  val to_string : t -> string
  (** Returns a string that should parse back to the given logic. *)

  val all: t
  (** All the smtlib2 logic parsable *)

  (** This functor/module is there to help detect logic by
      scanning/inspecting types and terms *)
  module Scan(V : Dolmen_intf.View.TFF.S) : sig

    exception Unknown_ty_builtin of V.ty_cst
    exception Unknown_term_builtin of V.term_cst
    (** Exception raised on builtins not allowed in SMT-LIB. *)

    type acc
    (** accumulator for computing minimal logics *)

    val nothing : acc
    (** The empty accumulator. *)

    val need_univ : acc -> bool
    (** Whether the univ type is used. *)

    val need_unit : acc -> bool
    (** Whether the unit type is used. *)

    val to_logic : acc -> t
    (** Tansform an accumulator into a logic *)

    val scan_ty : acc -> V.Ty.t -> acc
    (** Returns the minimal logic needed to typecheck a type. *)

    val scan_term_decl : in_adt:bool -> acc -> V.Term.Cst.t -> acc
    (** Returns the minimal logic needed to declare a term constant
        (i.e. including its type). *)

    val scan_term : acc -> V.Term.t -> acc
    (** Returns the minimal logic needed to typecheck a term. *)

    val add_datatypes : acc -> acc
    (** Add to the accumualtor the need to declare and use datatypes. *)

    val add_free_sort : acc -> acc
    (** Add to the accumualtor the need to declare new sorts/types. *)

    val add_free_funs : acc -> acc
    (** Add to the accumualtor the need to declare new functions.
        NOTE: declaring enw constants (i.e. functions with arity 0) is
        always allowed in all logics. This setting refers specifically
        to functions with an arity of at least 1. *)

  end

end

(** {2 All logics} *)

type t =
  | Auto
  (** Default case when no logic is specified or for languages which do not
      have a notion of different logics (i.e. the language only has one
      logic). *)
  | Smtlib2 of Smtlib2.t
  (** Smtlib2 logic. *)
(** Wrapper type to represent the different logics. *)

val print : Format.formatter -> t -> unit
(** Printer for logics. *)

