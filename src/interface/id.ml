
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

  val var : namespace
  (** Namespace for variables (when they can be syntatically distinguished from
      constants). *)

  val term : namespace
  (** The usual namespace for terms. *)

  val attr : namespace
  (** Namespace used for attributes (also called annotations) in smtlib. *)

  val decl : namespace
  (** Namespace used for declaration identifiers (for instance used
      to filter declarations during includes) *)

  val track : namespace
  (** Namespace used to tag and identify sub-terms occuring in files. *)

  val mk : namespace -> string -> t
  (** Make an identifier from its namespace and name. *)

  val indexed : namespace -> string -> string list -> t
  (** Make an indexed identifier from a namespace, basename and list of indexes. *)

  val qualified : namespace -> string list -> string -> t
  (** Make a qualified identifier from a namespace, a list of modules (a path),
      and a base name. *)

  val tracked : track:t -> namespace -> string -> t
  (** An identifier with an additional name for tracking purposes. *)

end

module type Response = Logic

module type Scope = sig

  type t
  (** The type of identifiers. *)

  type path
  (** Names for identifiers. *)

  val equal : t -> t -> bool
  (** Equality function *)

  val path : t -> path
  (** Identifier name. *)

  module Map : Map.S with type key = t
  (** A module for Maps on ids. *)

end

module type Scope_Full = sig

  include Scope
  (** We extend the regular `Scope` module. *)

  type namespace
  (** Namespaces for identifiers. *)

  val namespace : t -> namespace
  (** Return the namespace of an identifier. *)

end
