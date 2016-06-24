
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for Identifiers.
    This module defines Interfaces that implementation of identifiers must
    respect in order to be used to instantiated the corresponding
    language classes. *)

(** {2 Signature for Logic languages} *)

module type Logic = sig

  type t
  (** The type of identifiers. *)

  type namespace
  (** The type of namespaces. Namespaces are used to distinguish
      identifiers with the same name, but that occur in different
      contexts. For instance, in smtlib, sorts and terms live in
      different namespaces; so a term constant can have the same name
      as a sort, and still be syntactically different. *)

  val sort : namespace
  (** The namespace for sorts (also called types). Currently only used
      for smtlib. *)

  val term : namespace
  (** The usual namespace for terms. *)

  val attr : namespace
  (** Namespace used for attributes (also called annotations) in smtlib. *)

  val decl : namespace
  (** Namespace used for declaration identifiers (for instance used
      to filter declarations during includes) *)

  val mod_name : string -> namespace
  (** Namespace used by modules (for instance in dedulkti). *)

  val mk : namespace -> string -> t
  (** Make an identifier from its namespace and name. *)

end
