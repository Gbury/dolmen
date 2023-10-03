
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Smtlib2 extensions *)
(* ************************************************************************* *)

module Smtlib2 : sig

  type t

  val name : t -> string
  (** Name of an extension (should be suitable to use for e.g. option names) *)

  val enable : t -> unit
  val disable : t -> unit
  val set : t -> bool -> unit
  (** Enable/disable/set whether an extension is active. Only active extensions
      will be allowed by the [statement] function. *)

  val exts : unit -> t list
  (** The lists of extensions *)

  val create : name:string -> stmts:string list -> t
  (** Create a new extension. *)

  val maxsmt : t
  (** The extension for MaxSMT. *)

  val statement : string -> (?loc:Loc.t -> Term.t list -> Statement.t) option
  (** Called on statements of the form `(<id> <term>)` where `<id>` is
        not the name of a statement in the official smtlib specification. *)

end
