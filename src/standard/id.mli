
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard implementation of identifiers *)

(** {2 Type definitions} *)

type namespace =
  | Sort
  (** Namepsace for sorts and types (only used in smtlib) *)
  | Term
  (** Most used namespace, used for terms in general (and types outside smtlib). *)
  | Attr
  (** Namespace for attributes (also called annotations). *)
  | Decl
  (** Namespace used for naming declarations *)
  | Module of string
  (** Namespaces defined by modules (used for instance in dedukti). *)

type t = {
  ns : namespace;
  name : string;
}
(** The type of identifiers, basically a name together with a namespace. *)

(** {2 Implemented interfaces} *)

include Id_intf.Logic
  with type t := t
   and type namespace := namespace

(** {2 Additional functions} *)

val mk : namespace -> string -> t
(** Create an identifier. *)

val pp : Buffer.t -> t -> unit
val print : Format.formatter -> t -> unit
(** Printing functions. *)
