
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard implementation of identifiers *)

(** {2 Type definitions} *)

type value =
  | Integer
  (** Integers (in base 10 notation), e.g. ["123456789"] *)
  | Rational
  (** Rational (using quotient notation with '/'), e.g. ["123/456"] *)
  | Real
  (** Real (using decimal floating point notation with exponent),
      e.g. ["123.456e789"] *)
  | Binary
  (** Bitvector in binary notation, e.g. ["0b011010111010"] *)
  | Hexadecimal
  (** Bitvector in hexadecimal notation, e.g. ["0x9a23e5f"] *)
  | Bitvector
  (** Bitvector litteral. *)
  | String
  (** String litterals. *)
(** Types of lexical values typically encountered. *)

type namespace =
  | Var
  (** Namespace for variables. Not all variables are necessarily in
      this namespace, but ids in this namespace must be variables. *)
  | Sort
  (** Namepsace for sorts and types (only used in smtlib) *)
  | Term
  (** Most used namespace, used for terms in general (and types outside smtlib). *)
  | Attr
  (** Namespace for attributes (also called annotations). *)
  | Decl
  (** Namespace used for naming declarations/definitions/statements... *)
  | Track
  (** Namespace used to track specific values throughout some files. *)
  | Module of string
  (** Namespaces defined by modules (used for instance in dedukti). *)
  | Value of value
  (** The identifier is a value, encoded in a string. Examples include
      arithmetic constants (e.g. ["123456", "123/456", "123.456e789"]),
      bitvectors (i.e. binary notation), etc... *)
(** Namespaces, used to record the lexical scop in which an identifier
    was parsed. *)

type t = {
  ns : namespace;
  name : string;
}
(** The type of identifiers, basically a name together with a namespace. *)

(** {2 Implemented interfaces} *)

include Dolmen_intf.Id.Logic
  with type t := t
   and type namespace := namespace

(** {2 Usual comparison functions} *)

val hash    : t -> int
val equal   : t -> t -> bool
val compare : t -> t -> int
(** Usual functions for hashing, comparisons, equality. *)

(** {2 Additional functions} *)

val mk : namespace -> string -> t
(** Create an identifier. *)

val full_name : t -> string
(** Returns a full name for the identifier.
    NOTE: full names may not be unique and therefore not
          suitable for comparison of identifiers. *)

val pp : Buffer.t -> t -> unit
val print : Format.formatter -> t -> unit
(** Printing functions. *)

val split : t -> string list
(** Split an id into a list of strings (used notably for SMTLIb). *)



(** {2 Standard attributes} *)

val ac_symbol : t
(** Used to denote associative-commutative symbols. *)

val case_split : t
(** Used to annote axioms/antecedants which are case split in alt-ergo. *)

val theory_decl : t
(** Used to define theories; used primarily in alt-ergo where it affects
    what engine is used to trigger axioms in the theory. *)

val rwrt_rule : t
(** The tagged term is (or at least should be) a rewrite rule. *)

val tptp_role : t
(** The tagged statement has a tptp role. Should be used as a function
    symbol applied to the actual tptp role. *)

val tptp_kind : t
(** The tagged statement is of the given kind (e.g. tff, thf, ...).
    Should be used as a function symbol applied to the actual tptp kind. *)

