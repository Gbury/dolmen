
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** AST requirements for the iCNF format.
    iCNF is a very simple format intended to express CNFs (conjunctive normal forms)
    in the simplest format possible. Compared to dimacs, iCNF allows local
    assumptions, and does not require to declare the number of clauses and
    formulas. *)

module type Term = sig

  type t
  (** The type of terms. *)

  type location
  (** The type of locations. *)

  val atom : ?loc:location -> int -> t
  (** Make an atom from an non-zero integer. Positive integers denotes variables,
      and negative integers denote the negation of the variable corresponding to
      their absolute value. *)

end
(** Requirements for implementations of Dimacs terms. *)


module type Statement = sig

  type t
  (** The type of statements for iCNF. *)

  type term
  (** The type of iCNF terms. *)

  type location
  (** The type of locations. *)

  val p_inccnf : ?loc:location -> unit -> t
  (** header of an iCNF file. *)

  val clause : ?loc:location -> term list -> t
  (** Make a clause from a list of literals. *)

  val assumption : ?loc:location -> term list -> t
  (** Generate a solve instruction with the given list of assumptions. *)

end
(** Requirements for implementations of iCNF statements. *)


