
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for Extensions *)

module type Logic = sig

  type location
  (** The type of locations. *)

  type term
  (** The type of terms *)

  type statement
  (** The type of statements *)

  module Smtlib2 : sig

    val statement : string -> (?loc:location -> term list -> statement) option
    (** Called on statements of the form `(<id> <term>)` where `<id>` is
        not the name of a statement in the official smtlib specification. *)

  end

end
