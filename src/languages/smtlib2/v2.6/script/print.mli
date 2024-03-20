
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Printing of identifiers *)
(* ************************************************************************* *)

exception Cannot_print of string
(** Exception raised when some input cannot be printed due to lexical
    conventions. In that case, the string contains a message explaining
    why the printing failed. *)

val id : Format.formatter -> Dolmen_std.Name.t -> unit
(** Print an identifier, quoting it if necessary. *)


(* Printing of Terms and statements *)
(* ************************************************************************* *)

module Make
    (V : Dolmen_intf.View.FO.S)
    (S : Dolmen_intf.Scope.S
     with type id = <
         ty_var : V.Ty.Var.t;
         ty_cst : V.Ty.Cst.t;
         term_var : V.Term.Var.t;
         term_cst : V.Term.Cst.t;
       > Dolmen_intf.Scope.id)
  : sig


  (** {2 Statements} *)

  val set_logic : Format.formatter -> string -> unit
  (** Print a set-logic statement. *)

  val pop : Format.formatter -> int -> unit
  (** [pop fmt n] prints a statement that pops `n` levels.
      @raise Cannot_print if the provided level is non-positive *)

  val push : Format.formatter -> int -> unit
  (** [push fmt n] prints a statement that pushes `n` levels.
      @raise Cannot_print if the provided level is non-positive *)

  val reset : Format.formatter -> unit -> unit
  (** Print a `reset` statement. *)

  val reset_assertions : Format.formatter -> unit -> unit
  (** Print a `reset-assertion` statement. *)

  val get_unsat_core : Format.formatter -> unit -> unit
  (** Print a `get-unsat-core` statement. *)

  val get_unsat_assumptions : Format.formatter -> unit -> unit
  (** Print a `get-unsat-assumptions` statement. *)

  val get_proof : Format.formatter -> unit -> unit
  (** Print a `get-proof` statement. *)

  val get_model : Format.formatter -> unit -> unit
  (** Print a `get-model` statement. *)

  val get_assertions : Format.formatter -> unit -> unit
  (** Print a `get-assertions` statement. *)

  val get_assignment : Format.formatter -> unit -> unit
  (** Print a `get-assignment` statement. *)

  val exit : Format.formatter -> unit -> unit
  (** Print an `exit` statement. *)

end
