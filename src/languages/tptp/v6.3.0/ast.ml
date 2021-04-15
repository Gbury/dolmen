
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type Id = sig

  type t
  (** The type of identifiers *)

  type namespace
  (** The type for namespaces. *)

  val term : namespace
  (** Usual namespace, used for temrs, types and propositions. *)

  val decl : namespace
  (** Names used to refer to tptp phrases. These are used
      in declarations and include statement. *)

  val mk : namespace -> string -> t
  (** Make an identifier *)

end

module type Term = sig

  type t
  (** The type of terms. *)

  type id
  (** The type of identifiers *)

  type location
  (** The type of locations attached to terms. *)

  val eq_t      : ?loc:location -> unit -> t
  val neq_t     : ?loc:location -> unit -> t
  val not_t     : ?loc:location -> unit -> t
  val or_t      : ?loc:location -> unit -> t
  val and_t     : ?loc:location -> unit -> t
  val xor_t     : ?loc:location -> unit -> t
  val nor_t     : ?loc:location -> unit -> t
  val nand_t    : ?loc:location -> unit -> t
  val equiv_t   : ?loc:location -> unit -> t
  val implies_t : ?loc:location -> unit -> t
  val implied_t : ?loc:location -> unit -> t
  val pi_t      : ?loc:location -> unit -> t
  val sigma_t   : ?loc:location -> unit -> t
  val data_t    : ?loc:location -> unit -> t
  (** Predefined symbols in tptp. Symbols as standalone terms are necessary
      for parsing tptp's THF. {!implied_t} is reverse implication, and
      {!data_t} is used in tptp's annotations. {!pi_t} and {!sigma_t} are
      the encoding of forall and exists quantifiers as constant in higher-order
      logic. *)

  val colon : ?loc:location -> t -> t -> t
  (** Juxtaposition of terms, usually used for annotating terms with their type. *)

  val var      : ?loc:location -> id -> t
  (** Make a variable (in tptp, variable are syntaxically different from constants). *)

  val const    : ?loc:location -> id -> t
  (** Make a constant. *)

  val distinct : ?loc:location -> id -> t
  (** Make a constant whose name possibly contain special characters
      (All 'distinct' constants name are enclosed in quotes). *)

  val int      : ?loc:location -> string -> t
  val rat      : ?loc:location -> string -> t
  val real     : ?loc:location -> string -> t
  (** Constants that are syntaxically recognised as numbers. *)

  val apply : ?loc:location -> t -> t list -> t
  (** Application. *)

  val ite   : ?loc:location -> t -> t -> t -> t
  (** Conditional, of the form [ite condition then_branch els_branch]. *)

  val union   : ?loc:location -> t -> t -> t
  (** Union of types. *)

  val product : ?loc:location -> t -> t -> t
  (** Product of types, used for function types with more than one argument. *)

  val arrow   : ?loc:location -> t -> t -> t
  (** Function type constructor. *)

  val subtype : ?loc:location -> t -> t -> t
  (** Comparison of type (used in tptp's THF). *)

  val pi     : ?loc:location -> t list -> t -> t
  (** Dependant type constructor, used for polymorphic function types. *)

  val letin  : ?loc:location -> t list -> t -> t
  (** Local binding for terms. *)

  val forall : ?loc:location -> t list -> t -> t
  (** Universal propositional quantification. *)

  val exists : ?loc:location -> t list -> t -> t
  (** Existencial porpositional quantification. *)

  val lambda : ?loc:location -> t list -> t -> t
  (** Function construction. *)

  val choice : ?loc:location -> t list -> t -> t
  (** Indefinite description, also called choice operator. *)

  val description : ?loc:location -> t list -> t -> t
  (** Definite description. *)

  val sequent : ?loc:location -> t list -> t list -> t
  (** Sequents as terms, used as [sequents hyps goals]. *)

end

module type Statement = sig

  type t
  (** The type of statements. *)

  type id
  (** The type of identifiers *)

  type term
  (** The type of terms used in statements. *)

  type location
  (** The type of locations attached to statements. *)

  val annot : ?loc:location -> term -> term list -> term
  (** Terms as annotations for statements. *)

  val include_ : ?loc:location -> string -> id list -> t
  (** Include directive. Given the filename, and a list of
      names to import (an empty list means import everything). *)

  val tpi : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val thf : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val tff : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val fof : ?loc:location -> ?annot:term -> id -> string -> term -> t
  val cnf : ?loc:location -> ?annot:term -> id -> string -> term -> t
  (** TPTP statements, used for instance as [tff ~loc ~annot name role t].
      Instructs the prover to register a new directive with the given name,
      role and term. Current tptp roles are:
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

end

