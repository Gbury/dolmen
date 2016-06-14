
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** AST requirement for the Deduki format. *)

module type Term = sig

  type t
  (** The type of terms. *)

  type location
  (** The type of locations. *)

  type namespace
  (** Namespace for identifiers *)

end
(** Implementation requirements for Dedukti terms. *)

module type Statement = sig

  type t
  (** The type of statements. *)

  type term
  (** The type of terms. *)

  type location
  (** The type of locations. *)

  val mk_module : string -> t
  (** Gives the name of a module. Typically this will be the first statement
      of a file.*)



end
(** Implementation requirement for Dedukti statements. *)

