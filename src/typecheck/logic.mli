
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

  val all: t
  (** All the smtlib2 logic parsable *)
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

