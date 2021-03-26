
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** AST requirements for the Dimacs format.
    Dimacs is a very simple format intended to express CNFs (conjunctive normal forms)
    in the simplest format possible. *)

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
  (** The type of statements for dimacs. *)

  type term
  (** The type of dimacs terms. *)

  type location
  (** The type of locations. *)

  val p_cnf : ?loc:location -> int -> int -> t
  (** Header of a dimacs file. First argument is the number of variables,
      second is the number of clauses. *)

  val clause : ?loc:location -> term list -> t
  (** Make a clause from a list of literals. *)

end
(** Requirements for implementations of Dimacs statements. *)


