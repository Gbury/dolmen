
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for statements.
    This module defines interfaces for statements, i.e top-level
    declarations in files. *)

module type Logic = sig

  (** Signature used by the Logic class, which parses languages
      such as tptp, smtlib, etc...
      Statements of dirrent languages currently have a lot less in common
      than terms, so this interface looks a lot more like a patchwork
      of different logical framework directives than it should.
  *)

  include Base_intf.S

  type term
  (** The type of terms used in statements. *)

  (** {2 Optional infos for statements} *)

  val attr  : ?loc:location -> string -> term
  val annot : ?loc:location -> term -> term list -> term
  (** Constructors for attribute/annotations. Annotations are used in TPTP, and
      attributes in the Zipperposition format. *)

  (** {2 Generic statements} *)

  val include_ : ?loc:location -> string -> term list -> t
  (** Inlcude directive. [include file l] means to include in the current scope
      the directives from file [file] that appear in [l]. If [l] is the empty list,
      all directives should be imported. *)

  (** {2 Dimacs Statements} *)

  val clause      : ?loc:location -> term list -> t
  (** Add to the current set of assertions the given list of terms as a clause. *)

  (** {2 Smtlib statements} *)

  val pop         : ?loc:location -> int -> t
  val push        : ?loc:location -> int -> t
  (** Directives for manipulating the set of assertions. Push directives
      creates backtrack point that can be reached using Pop directives. *)

  val assert_     : ?loc:location -> term -> t
  (** Add an assertion to the current set of assertions. *)

  val check_sat   : ?loc:location -> unit -> t
  (** Directive that instructs the prover to solve the current set of assertions. *)

  val set_logic   : ?loc:location -> string -> t
  (** Set the logic to be used for solving. *)

  val get_info    : ?loc:location -> string -> t
  val set_info    : ?loc:location -> string * term option -> t
  (** Getter and setter for various informations (see smtlib manual). *)

  val get_option  : ?loc:location -> string -> t
  val set_option  : ?loc:location -> string * term option -> t
  (** Getter and setter for prover options (see smtlib manual). *)

  val type_decl   : ?loc:location -> string -> int -> t
  (** Type declaration. [type_decl s n] declare [s] as a type constructor with
      arity [n]. *)

  val type_def    : ?loc:location -> string -> term list -> term -> t
  (** Type definition. [type_def f args body] declare that [f(args) = body],
      i.e any occurence of "f(l)" should be replaced by [body] where the "args" have been
      substituted by their corresponding value in [l]. *)

  val fun_decl    : ?loc:location -> string -> term list -> term -> t
  (** Symbol declaration. [fun_decl f args ret] defines [f] as a function
      which takes arguemnts of type as described in [args] and which returns
      a value of type [ret]. *)

  val fun_def     : ?loc:location -> string -> term list -> term -> term -> t
  (** Symbol definition. [fun_def f args ret body] means that "f(args) = (body : ret)",
      i.e f is a function symbol with arguments [args], and which returns the value
      [body] which is of type [ret]. *)

  val get_proof       : ?loc:location -> unit -> t
  (** If the last call to [check_sat] returned UNSAT, then instruct the prover to return
      the proof of unsat. *)

  val get_unsat_core  : ?loc:location -> unit -> t
  (** If the last call to [check_sat] returned UNSAT, then instruct the prover to return
      the unsat core of the unsatisfiability proof, i.e the smallest set of assertions
      needed to prove [false]. *)

  val get_value       : ?loc:location -> term list -> t
  (** Instructs the prover to return the values of the given closed quantifier-free terms. *)

  val get_assignment  : ?loc:location -> unit -> t
  (** Instructs the prover to return truth assignemnt for labelled formulas (see smtlib manual
      for more information). *)

  val get_assertions  : ?loc:location -> unit -> t
  (** Instructs the prover to print all current assertions. *)

  val exit : ?loc:location -> unit -> t
  (** Exit directive (used in interactive mode). *)

  (** {2 TPTP Statements} *)

  val tpi : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val thf : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val tff : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val fof : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val cnf : ?loc:location -> ?annot:term -> term -> string -> term -> t
  (** TPTP directives. [tptp name role t] instructs the prover to register
      a new directive with the given name, role and term. Current tptp roles are:
      - ["axiom", "hypothesis", "definition", "lemma", "theorem"] acts
        as new assertions/declartions
      - ["assumption", "conjecture"] are proposition that need to be proved,
        and then can be used to prove other propositions. They are equivalent
        to the following sequence of smtlib statements:
        {ul
          {- [push 1]}
          {- [assert (not t)]}
          {- [check_sat]}
          {- [pop 1]}
          {- [assert t]}
        }
      - ["negated_conjecture"] is the same as ["conjecture"], but the given proposition
        is false (i.e its negation is the proposition to prove).
      - ["type"] declares a new symbol and its type
      - ["plain", "unknown", "fi_domain", "fi_functors", "fi_predicates"] are valid
        roles with no specified semantics
      - any other role is an error
  *)

  (** {2 Zipperposition statements} *)

  val inductive   : ?loc:location -> string -> term list -> (string * term list) list -> t
  (** Inductive type definitions. [inductive name vars l] defines aan inductive type [name],
      with polymorphic variables [vars], and with a list of inductive constructors [l]. *)

  val data        : ?loc:location -> t list -> t
  (** Packs a list of mutually recursive inductive type declarations. into a single declaration. *)

  val decl        : ?loc:location -> string -> term -> t
  (** Symbol declaration. [decl name ty] declares a new symbol [name] with type [ty]. *)

  val definition  : ?loc:location -> string -> term -> term -> t
  (** Symbol definition. [def name ty term] defines a new symbol [name] of type [ty]
      which is equal to [term]. *)

  val goal        : ?loc:location -> ?attr:term -> term -> t
  (** The goal, i.e the propositional formula to prove. *)

  val assume      : ?loc:location -> ?attr:term -> term -> t
  (** Adds an hypothesis. *)

  val rewrite     : ?loc:location -> ?attr:term -> term -> t
  (** Declare a rewrite rule, i.e a universally quantified equality or equivalence that
      can be oriented according to a specific ordering. *)

end
