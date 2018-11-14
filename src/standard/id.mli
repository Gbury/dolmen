
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard implementation of identifiers *)

(** {2 Type definitions} *)

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
  | Module of string
  (** Namespaces defined by modules (used for instance in dedukti). *)

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


(** {2 Standard attributes} *)

val rwrt_rule : t
(** The tagged term is (or at least should be) a rewrite rule. *)

val tptp_role : t
(** The tagged statement has a tptp role. Should be used as a function
    symbol applied to the actual tptp role. *)


