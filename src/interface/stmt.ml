
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

  type t
  (** The type of statements. *)

  type id
  (** The type of identifiers. *)

  type term
  (** The type of terms used in statements. *)

  type location
  (** The type of locations attached to statements. *)

  (** {2 Optional infos for statements} *)

  val annot : ?loc:location -> term -> term list -> term
  (** Constructors for annotations. Annotations are mainly used in TPTP. *)

  (** {2 Generic statements} *)

  val import   : ?loc:location -> string -> t
  (** Import directive. Same as [include_] but without filtering on the
      statements to import. *)

  val include_ : ?loc:location -> string -> id list -> t
  (** Inlcude directive. [include file l] means to include in the current scope
      the directives from file [file] that appear in [l]. If [l] is the empty list,
      all directives should be imported. *)

  (** {2 Alt-ergo Statements} *)

  val logic : ?loc:location -> ac:bool -> id list -> term -> t
  (** Functions type definition. Allows to specify whether a list of symbol is ac or not *)

  val record_type : ?loc:location -> id -> term list -> (id * term) list -> t
  (** Declares a new record type, with first a list of type variables,
      and then the list of the record fields. *)

  val abstract_type : ?loc:location -> id -> term list -> t
  (** Declare a new abstract type, quantified over the given list of
      type variables. *)

  val algebraic_type : ?loc:location -> id -> term list -> (id * term list) list -> t
  (** Defines a new algebraic datatype, quantified over the lsit of type variables,
      and with a list of cases each containing a constructor id and a list of
      fields. *)

  val rec_types : ?loc:location -> t list -> t
  (** Pack together a list of mutually recursive type definitions. *)

  val axiom : ?loc:location -> id -> term -> t
  (** Create a axiom. *)

  val case_split : ?loc:location -> id -> term -> t
  (** Create a case split. *)

  val theory : ?loc:location -> id -> id -> t list -> t
  (** Create a theory, extending another, with the given list of declarations. *)

  val rewriting : ?loc:location -> id -> term list -> t
  (** Create a set of rewriting rules. *)

  val prove_goal : ?loc:location -> id -> term -> t
  (** Goal declaration. *)


  (** {2 Dimacs&iCNF Statements} *)

  val p_cnf       : ?loc:location -> int -> int -> t
  (** Header of dimacs files. First argument is the number of variables,
      second is the number of clauses. *)

  val p_inccnf       : ?loc:location -> unit -> t
  (** Header of iCNF files. *)

  val clause      : ?loc:location -> term list -> t
  (** Add to the current set of assertions the given list of terms as a clause. *)

  val assumption  : ?loc:location -> term list -> t
  (** Solve the current set of assertions, with the given assumptions. *)

  (** {2 Smtlib statements} *)

  val pop         : ?loc:location -> int -> t
  val push        : ?loc:location -> int -> t
  (** Directives for manipulating the set of assertions. Push directives
      creates backtrack point that can be reached using Pop directives. *)

  val reset_assertions : ?loc:location -> unit -> t
  (** Reset all assertions that hase been pushed. *)

  val assert_     : ?loc:location -> term -> t
  (** Add an assertion to the current set of assertions. *)

  val check_sat   : ?loc:location -> term list -> t
  (** Directive that instructs the prover to solve the current set of assertions,
      undr some local assumptions. *)

  val set_logic   : ?loc:location -> string -> t
  (** Set the logic to be used for solving. *)

  val get_info    : ?loc:location -> string -> t
  val set_info    : ?loc:location -> term -> t
  (** Getter and setter for various informations (see smtlib manual). *)

  val get_option  : ?loc:location -> string -> t
  val set_option  : ?loc:location -> term -> t
  (** Getter and setter for prover options (see smtlib manual). *)

  val type_decl   : ?loc:location -> id -> int -> t
  (** Type declaration. [type_decl s n] declare [s] as a type constructor with
      arity [n]. *)

  val type_def    : ?loc:location -> id -> id list -> term -> t
  (** Type definition. [type_def f args body] declare that [f(args) = body],
      i.e any occurence of "f(l)" should be replaced by [body] where the "args" have been
      substituted by their corresponding value in [l]. *)

  val datatypes   : ?loc:location -> (id * term list * (id * term list) list) list -> t
  (** Inductive type definitions.
      TODO: some more documentation. *)

  val fun_decl    : ?loc:location -> id -> term list -> term list -> term -> t
  (** Symbol declaration. [fun_decl f vars args ret] defines [f] as a function
      which takes arguments of type as described in [args] and which returns
      a value of type [ret]. *)

  val fun_def     : ?loc:location -> id -> term list -> term list -> term -> term -> t
  (** Symbol definition. [fun_def f vars args ret body] means that "f(args) = (body : ret)",
      i.e f is a function symbol with arguments [args], and which returns the value
      [body] which is of type [ret]. *)

  val funs_def_rec : ?loc:location -> (id * term list * term list * term * term) list -> t
  (** Define a list of mutually recursive functions. Each functions has the same
      definition as in [fun_def] *)

  val get_proof       : ?loc:location -> unit -> t
  (** If the last call to [check_sat] returned UNSAT, then instruct the prover to return
      the proof of unsat. *)

  val get_unsat_core  : ?loc:location -> unit -> t
  (** If the last call to [check_sat] returned UNSAT, then instruct the prover to return
      the unsat core of the unsatisfiability proof, i.e the smallest set of assertions
      needed to prove [false]. *)

  val get_unsat_assumptions : ?loc:location -> unit -> t
  (** If the last call to [check_sat] returned UNSAT, then instruct the prover to
      return a subset of the local assumptions that is sufficient to deduce UNSAT. *)

  val get_model       : ?loc:location -> unit -> t
  (** If the last call to [check_sat] returned SAT, then return the associated model. *)

  val get_value       : ?loc:location -> term list -> t
  (** Instructs the prover to return the values of the given closed quantifier-free terms. *)

  val get_assignment  : ?loc:location -> unit -> t
  (** Instructs the prover to return truth assignemnt for labelled formulas (see smtlib manual
      for more information). *)

  val get_assertions  : ?loc:location -> unit -> t
  (** Instructs the prover to print all current assertions. *)

  val echo : ?loc:location -> string -> t
  (** Print the given sting. *)

  val reset : ?loc:location -> unit -> t
  (** Full reset of the prover state. *)

  val exit : ?loc:location -> unit -> t
  (** Exit directive (used in interactive mode). *)

  (** {2 TPTP Statements} *)

  val tpi : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val thf : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val tff : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val fof : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val cnf : ?loc:location -> ?annot:term -> id -> string -> term -> t
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

  val data        : ?loc:location -> ?attrs:term list -> t list -> t
  (** Packs a list of mutually recursive inductive type declarations into a
      single statement. *)

  val defs        : ?loc:location -> ?attrs:term list -> t list -> t
  (** Packs a list of mutually recursive definitions into a single statement. *)

  val rewrite     : ?loc:location -> ?attrs:term list -> term -> t
  (** Declare a rewrite rule, i.e a universally quantified equality or equivalence that
      can be oriented according to a specific ordering. *)

  val goal        : ?loc:location -> ?attrs:term list -> term -> t
  (** The goal, i.e the propositional formula to prove. *)

  val assume      : ?loc:location -> ?attrs:term list -> term -> t
  (** Adds an hypothesis. *)

  val lemma       : ?loc:location -> ?attrs:term list -> term -> t
  (** Lemmas. *)

  val decl        : ?loc:location -> ?attrs:term list -> id -> term -> t
  (** Symbol declaration. [decl name ty] declares a new symbol [name] with type [ty]. *)

  val definition  : ?loc:location -> ?attrs:term list -> id -> term -> term list -> t
  (** Symbol definition. [def name ty term] defines a new symbol [name] of type [ty]
      which is equal to [term]. *)

  val inductive   :
    ?loc:location -> ?attrs:term list -> id -> term list -> (id * term list) list -> t
  (** Inductive type definitions. [inductive name vars l] defines an inductive type [name],
      with polymorphic variables [vars], and with a list of inductive constructors [l]. *)

end
