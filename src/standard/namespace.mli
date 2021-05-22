
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)


(** {2 Type and std functions} *)
(*  ************************************************************************* *)

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

type t =
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
  | Value of value
  (** The identifier is a value, encoded in a string. Examples include
      arithmetic constants (e.g. ["123456", "123/456", "123.456e789"]),
      bitvectors (i.e. binary notation), etc... *)
(** Namespaces, used to record the lexical scop in which an identifier
    was parsed. *)

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
(** Std functions *)

val print : Format.formatter -> t -> unit
(** Printing function. *)

module Map : Maps.S with type key := t
(** Maps on namespaces *)


(** {2 Creation} *)
(*  ************************************************************************* *)

val var : t
(** The variable namespace. *)

val sort : t
(** The sort namespace. *)

val term : t
(** The term namespace. *)

val attr : t
(** Teh attribute namespace. *)

val decl : t
(** The declaration namespace. *)

val track : t
(** Namespace used for identifiers used for tracking/special identification. *)



