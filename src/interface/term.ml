
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for Terms.
    This module defines Interfaces that implementation of terms must
    respect in order to be used to instantiated the corresponding
    language classes. *)

(** {2 Signature for Logic languages} *)

module type Logic = sig

  (** Signature used by the Logic class, which parses languages
      such as tptp, smtlib, etc...
      Mainly used to parse first-order terms, it is also used to
      parse tptp's THF language, which uses higher order terms, so
      some first-order constructs such as conjunction, equality, etc...
      also need to be represented by standalone terms.
  *)

  type t
  (** The type of terms. *)

  type id
  (** The type of identifiers used for constants. *)

  type location
  (** The type of locations attached to terms. *)

  (** {3 Predefined terms} *)

  val eq_t      : ?loc:location -> unit -> t
  val neq_t     : ?loc:location -> unit -> t
  (** The terms representing equality and disequality, respectively. *)

  val wildcard  : ?loc:location -> unit -> t
  (** The wildcard term, usually used in place of type arguments
      to explicit polymorphic functions to not explicit types that
      can be inferred by the type-checker. *)

  val tType     : ?loc:location -> unit -> t
  (** The type of types, defined as specific token by the Zipperposition format;
      in other languages, will be represented as a constant (the "$tType" constant
      in tptp for instance). Used to define new types, or quantify type variables
      in languages that support polymorphism. *)

  val ty_int    : ?loc:location -> unit -> t
  (** The type of integers, defined as a specific token by the Zipperposition format;
      in other languages, it might be represented as a constant with a specific name
      (for isntance, tptp's "$int") .*)

  val prop      : ?loc:location -> unit -> t
  (** The type of propositions. Also defined as a lexical token by the Zipperposition
      format. Will be defined as a constant in most other languages (for instance,
      "$o" in tptp). *)

  val true_     : ?loc:location -> unit -> t
  val false_    : ?loc:location -> unit -> t
  (** The constants for the true and false propositional constants. Again defined
      as lexical token in the Zipperposition format, while treated as a constant
      in other languages ("$true" in tptp). *)

  val not_t     : ?loc:location -> unit -> t
  val or_t      : ?loc:location -> unit -> t
  val and_t     : ?loc:location -> unit -> t
  val xor_t     : ?loc:location -> unit -> t
  val nor_t     : ?loc:location -> unit -> t
  val nand_t    : ?loc:location -> unit -> t
  val equiv_t   : ?loc:location -> unit -> t
  val implied_t : ?loc:location -> unit -> t
  val implies_t : ?loc:location -> unit -> t
  (** Standard logical connectives viewed as terms. [implies_t] is usual
      right implication, i.e [apply implies_t \[p; q\] ] is "p implies q",
      while [apply implied_t \[p; q \]] means "p is implied by q" or
      "q implies p". *)

  val data_t    : ?loc:location -> unit -> t
  (** Term without semantic meaning, used for creating "data" terms.
      Used in tptp's annotations, and with similar meaning as smtlib's
      s-expressions (as used in the [sexpr] function defined later). *)


  (** {3 Terms leaf constructors} *)

  val var      : ?loc:location -> id -> t
  val const    : ?loc:location -> id -> t
  (** Variable and constant constructors. While in some languages
      they can distinguished at the lexical level (in tptp for instance),
      in most languages, it is an issue dependant on scoping rules,
      so terms parsed from an smtlib file will have all variables
      parsed as constants. *)

  val atom     : ?loc:location -> int -> t
  (** Atoms are used for dimacs cnf parsing. Positive integers denotes variables,
      and negative integers denote the negation of the variable corresponding to
      their absolute value. *)

  val distinct : ?loc:location -> id -> t
  (** Used in tptp to specify constants different from other constants, for instance the
      'distinct' "Apple" should be syntactically different from the "Apple"
      constant. Can be safely aliased to the [const] function as the
      [distinct] function is always given strings already enclosed with quotes,
      so in the example above, [const] would be called with ["Apple"] as
      string argument, while [distinct] would be called with the string ["\"Apple\""] *)

  val int      : ?loc:location -> string -> t
  val rat      : ?loc:location -> string -> t
  val real     : ?loc:location -> string -> t
  val hexa     : ?loc:location -> string -> t
  val binary   : ?loc:location -> string -> t
  (** Constructors for words defined as numeric formats by the languages
      specifications. These also can be safely aliased to [const]. *)


  (** {3 Term constructors} *)

  val colon : ?loc:location -> t -> t -> t
  (** Represents juxtaposition of two terms, usually denoted "t : t'"
      in most languages, and mainly used to annotated terms with their
      supposed, or defined, type. *)

  val eq    : ?loc:location -> t -> t -> t
  val not_  : ?loc:location -> t -> t
  val or_   : ?loc:location -> t list -> t
  val and_  : ?loc:location -> t list -> t
  val imply : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t
  (** Proposition construction functions. The conjunction and disjunction
      are n-ary instead of binary mostly because they are in smtlib (and
      that is subsumes the binary case). *)

  val apply : ?loc:location -> t -> t list -> t
  (** Application constructor, seen as higher order application
      rather than first-order application for the following reasons:
      being able to parse tptp's THF, having location attached
      to function symbols. *)

  val ite   : ?loc:location -> t -> t -> t -> t
  (** Conditional constructor, both for first-order terms and propositions.
      Used in the following schema: [ite condition then_branch else_branch]. *)

  val match_ : ?loc:location -> t -> (t * t) list -> t
  (** Pattern matching. The first term is the term to match,
      and each tuple in the list is a match case, which is a pair
      of a pattern and a match branch. *)

  val pi     : ?loc:location -> t list -> t -> t
  val letin  : ?loc:location -> t list -> t -> t
  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t
  val lambda : ?loc:location -> t list -> t -> t
  val choice : ?loc:location -> t list -> t -> t
  val description : ?loc:location -> t list -> t -> t
  (** Binders for variables. Takes a list of terms as first argument
      for simplicity, the lists will almost always be a list of variables,
      optionally typed using the [colon] term constructor.
      - Pi is the polymorphic type quantification, for instance
        the polymorphic identity function has type: "Pi alpha. alpha -> alpha"
      - Letin is local binding, takes a list of equality of equivalences
        whose left hand-side is a variable.
      - Forall is universal quantification
      - Exists is existential quantification
      - Lambda is used for function construction
      - Choice is the choice operator, also called indefinite description, or
        also epsilon terms, i.e "Choice x. p(x)" is one "x" such that "p(x)"
        is true.
      - Description is the definite description, i.e "Description x. p(x)"
        is the {b only} "x" that satisfies p.
  *)

  (** {3 Type constructors} *)

  val arrow   : ?loc:location -> t -> t -> t
  (** Function type constructor, for curryfied functions. Functions
      that takes multiple arguments in first-order terms (and so
      naturally not curryfied) will take a product as only argument
      (see the following [product] function). *)

  val product : ?loc:location -> t -> t -> t
  (** Product type constructor, used for instance in the types of
      functions that takes multiple arguments in a non-curry way. *)

  val union   : ?loc:location -> t -> t -> t
  (** Union type constructor, currently used in tptp's THF format. *)

  val subtype : ?loc:location -> t -> t -> t
  (** Subtype relation for types. *)

  (** {3 Arithmetic constructors} *)

  val uminus : ?loc:location -> t -> t
  (** Arithmetic unary minus. *)

  val add    : ?loc:location -> t -> t -> t
  (** Arithmetic addition. *)

  val sub    : ?loc:location -> t -> t -> t
  (** Arithmetic substraction. *)

  val mult   : ?loc:location -> t -> t -> t
  (** Arithmetic multiplication. *)

  val lt     : ?loc:location -> t -> t -> t
  (** Arithmetic "lesser than" comparison (strict). *)

  val leq    : ?loc:location -> t -> t -> t
  (** Arithmetic "lesser or equal" comparison. *)

  val gt     : ?loc:location -> t -> t -> t
  (** Arithmetic "greater than" comparison (strict). *)

  val geq    : ?loc:location -> t -> t -> t
  (** Arithmetic "greater or equal" comparison. *)


  (** {3 Special constructions} *)

  val quoted  : ?loc:location -> string -> t
  (** Create an attribute from a quoted string (in Zf). *)

  val sequent : ?loc:location -> t list -> t list -> t
  (** Sequents as terms *)

  val annot   : ?loc:location -> t -> t list -> t
  (** Attach a list of attributes (also called annotations) to a term. Attributes
      have no logical meaning (they can be safely ignored), but may serve to give
      hints or meta-information. *)

  val sexpr   : ?loc:location -> t list -> t
  (** S-expressions (for smtlib attributes), should probably be related
      to the [data_t] term. *)

end
