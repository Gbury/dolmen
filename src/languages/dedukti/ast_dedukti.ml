
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** AST requirement for the Deduki format. *)

module type Id = sig

  type t
  (** The type of terms. *)

  type namespace
  (** Namespace for identifiers *)

  val term : namespace
  (** The term namespace. *)

  val mk : namespace -> string -> t
  (** Create a simple identifier. *)

  val qualified : namespace -> string list -> string -> t
  (** Create a qualified identifier. *)

end

module type Term = sig

  type t
  (** The type of terms. *)

  type id
  (** The type of identifiers. *)

  type location
  (** The type of locations. *)

  val mk : ?loc:location -> id -> t
  (** Make a term from an identifier. *)

end
(** Implementation requirements for Dedukti terms. *)

module type Statement = sig

  type t
  (** The type of statements. *)

  type term
  (** The type of terms. *)

  type location
  (** The type of locations. *)

  val module_start : string -> t
  (** Gives the name of a module. Typically this will be the first statement
      of a file.*)



end
(** Implementation requirement for Dedukti statements. *)

