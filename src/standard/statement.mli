
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard imlplementation of statements.
    This module provides a reasonable and standard implementation of statements,
    that can directly be used to instantiated the various functors of the dolmen library.
    These statements are closer to smtlib statements than to other languages statements
    because it is easier to express other languages statements using smtlib's than the
    other way around. Still, a generalisation of smtlib statements was needed so as not
    to lose some important distinctions between conjectures and assertions for instance.
*)


(** {2 Type definitions} *)

type term = Term.t
type location = Loc.t
(** Type aliases for readability. *)

type abstract = {
  id : Id.t;
  ty : term;
  loc : location;
  attrs : term list;
}
(** The type for abstract type definitions. *)

type inductive = {
  id : Id.t;
  vars : term list;
  cstrs : (Id.t * term list) list;
  loc : location;
  attrs : term list;
}
(** The type for inductive type declarations. The "vars" field if used
    to store polymorphic variables of the inductive type. For instance,
    a polymorphic type for lists would have a single variable "a".
    The constructors each have a name and a list of concrete arguments types
    (they all implicitly take as many type arguments as there are variables).
    So, for instance, the polymorphic list type would have two constructors:
    - ["Nil", \[\]]
    - ["Cons", \[var "a"\]]
*)

type record = {
  id : Id.t;
  vars : term list;
  fields : (Id.t * term) list;
  loc : location;
  attrs : term list;
}
(** The type of record definitions. *)

type decl =
  | Abstract of abstract
  | Record of record
  | Inductive of inductive (**)
(** Type definitions, type declarations, and term declaration. *)

type def = {
  id : Id.t;
  vars : term list;
  params : term list;
  ret_ty : term;
  body : term;
  loc : location;
  attrs : term list;
}
(** Term definition. *)

type 'a group = {
  contents : 'a list;
  recursive : bool;
}
(** Groups of declarations or definitions, which can be recursive or not. *)

type defs = def group
type decls = decl group
(** Convenient aliases *)

type local = { hyps: term list; goals: term list }
(** Local hypothesis and consequents used by [Prove]. *)

type descr =
  | Pack of t list
  (** Pack a list of statements that have a semantic meaning (for instance
      a list of mutually recursive inductive definitions). *)

  | Pop of int
  (** Pop the stack of assertions as many times as specified. *)
  | Push of int
  (** Push as many new levels on the stack of assertions as specified. *)
  | Reset_assertions
  (** Reset all assertions. *)

  | Plain of term
  (** A plain statement containing a term with no defined semantics. *)

  | Prove of local
  (** Try and prove the current consequents or local consequents, under
      some local assumptions. *)

  | Clause of term list
  (** Add the given clause on the left side of the current sequent. *)
  | Antecedent of term
  (** Add the given proposition on the left of the current sequent. *)
  | Consequent of term
  (** Add the given proposition on the right of the current sequent.*)

  | Include of string
  (** File include, qualified include paths, if any, are stored in the attribute. *)

  | Set_logic of string
  (** Set the logic to use for proving. *)

  | Get_info of string
  (** Get required information. *)
  | Set_info of term
  (** Set the information value. *)

  | Get_option of string
  (** Get the required option value. *)
  | Set_option of term
  (** Set the option value. *)

  | Defs of def group
  (** Symbol definition, i.e the symbol is equal to the given term. *)
  | Decls of decl group
  (** A list of potentially recursive type definitions. *)

  | Get_proof
  (** Get the proof of the last sequent (if it was proved). *)
  | Get_unsat_core
  (** Get the unsat core of the last sequent. *)
  | Get_unsat_assumptions
  (** Get the local assumptions in the unsat core of the last sequent. *)
  | Get_model
  (** Get the current model of the prover. *)
  | Get_value of term list
  (** Get the value of some terms in the current model of the prover. *)
  | Get_assignment
  (** Get the assignment of labbeled formulas (see smtlib manual). *)

  | Get_assertions
  (** Get the current set of assertions. *)

  | Echo of string
  (** Prints the string. *)
  | Reset
  (** Full reset of the prover to its initial state. *)
  | Exit
  (** Exit the interactive loop. *)

and t = {
  id : Id.t option;
  descr : descr;
  attrs : term list;
  loc : location;
}
(** The type of statements. Statements have optional location and attributes (or annotations).
    Additionally the each have a name (which mainly comes from tptp statements), that can
    very well be the empty string (and so it is likely {b not} unique). *)

(** {2 Implemented interfaces} *)

include Dolmen_intf.Stmt.Logic
  with type t := t
   and type id := Id.t
   and type term := term
   and type location := location

(** {2 Additional functions} *)

val mk_decls :
  ?loc:location -> ?attrs:term list -> recursive:bool -> decl list -> t
(** Create a group of declarations *)

val mk_defs :
  ?loc:location -> ?attrs:term list -> recursive:bool -> def list -> t
(** Create a group of declarations *)

val def :
  ?loc:location -> Id.t -> vars:term list -> params:term list ->
  term -> term -> def
(** Create a single definition. *)

val group : recursive:bool -> 'a list -> 'a group
(** Make a group 'usually of declarations/definitions. *)

val prove : ?loc:location -> unit -> t
(** Emit a [Prove] statement. *)

val pack : ?id:Id.t -> ?loc:location -> ?attrs:term list -> t list -> t
(** Pack a list of statements into a single one. *)


(** {2 Printing functions} *)

val print : Format.formatter -> t -> unit
(** Printing functions for statements. *)

val print_decl : Format.formatter -> decl -> unit
(* Printer for declarations. *)

val print_def : Format.formatter -> def -> unit
(* Printer for declarations. *)

val print_group :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a group -> unit
(* Printer for groups. *)

