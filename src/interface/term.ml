
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for Terms.
    This module defines Interfaces that implementation of terms must
    respect in order to be used to instantiated the corresponding
    language classes. *)

(** {2 Signature for Parsing Logic languages} *)

module type Logic = sig

  (** Signature used by the Logic class, which parses languages
      such as tptp, smtlib, etc...
      Mainly used to parse first-order terms, it is also used to
      parse tptp's THF language, which uses higher order terms, so
      some first-order constructs such as conjunction, equality, etc...
      also need to be represented by standalone terms. *)

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

  val prop      : ?loc:location -> unit -> t
  (** The type of propositions. Also defined as a lexical token by the Zipperposition
      format. Will be defined as a constant in most other languages (for instance,
      "$o" in tptp). *)

  val bool   : ?loc:location -> unit -> t
  (** The type of boolean, defined as a specific token by the Alt-ergo format;
      in other languages, it might be represented as a constant with a specific name. *)

  val ty_unit   : ?loc:location -> unit -> t
  (** The type unit, defined as a specific token by the Alt-ergo format;
      in other languages, it might be represented as a constant with a specific name. *)

  val ty_int    : ?loc:location -> unit -> t
  (** The type of integers, defined as a specific token by the Zipperposition and Alt-ergo
      formats;
      in other languages, it might be represented as a constant with a specific name
      (for isntance, tptp's "$int") .*)

  val ty_real   : ?loc:location -> unit -> t
  (** The type of integers, defined as a specific token by the Alt-ergo format;
      in other languages, it might be represented as a constant with a specific name
      (for isntance, tptp's "$int") .*)

  val ty_bitv   : ?loc:location -> int -> t
  (** The type of bitvectors of the given constant length, defined as a specifi token
      by the Alt-ergo format;
      in other languages, it might be represented as a constant with a specific name
      (for isntance, smtlib(s "bitv") .*)

  val void      : ?loc:location -> unit -> t
  (** The only value of type unit, defined as a specific token by the Alt-ergo format. *)

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
  val pi_t      : ?loc:location -> unit -> t
  val sigma_t   : ?loc:location -> unit -> t
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

  val str      : ?loc:location -> string -> t
  val int      : ?loc:location -> string -> t
  val rat      : ?loc:location -> string -> t
  val real     : ?loc:location -> string -> t
  val hexa     : ?loc:location -> string -> t
  val binary   : ?loc:location -> string -> t
  (** Constructors for words defined as numeric or string formats by the languages
      specifications. These also can be safely aliased to [const], but then the
      provenance information is lost, which might complicate the task of a
      type-checker. *)

  val bitv     : ?loc:location -> string -> t
  (** Bitvetor literal, defined as a specific token in Alt-ergo;
      Expects a decimal integer in the string to be extended as a bitvector. *)

  (** {3 Term constructors} *)

  val colon : ?loc:location -> t -> t -> t
  (** Represents juxtaposition of two terms, usually denoted "t : t'"
      in most languages, and mainly used to annotated terms with their
      supposed, or defined, type. *)

  val eq    : ?loc:location -> t -> t -> t
  val neq   : ?loc:location -> t list -> t
  (** Equality and dis-equality of terms. *)

  val not_  : ?loc:location -> t -> t
  val or_   : ?loc:location -> t list -> t
  val and_  : ?loc:location -> t list -> t
  val xor   : ?loc:location -> t -> t -> t
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
  val par    : ?loc:location -> t list -> t -> t
  val letin  : ?loc:location -> t list -> t -> t
  val letand : ?loc:location -> t list -> t -> t
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
        whose left hand-side is a variable. Letand is the parrallel version
        of Letin.
      - Forall is universal quantification
      - Par is universal quantification over type variables specifically
        (i.e. the same as forall, but only for a list of type variables,
        which thus may omit the [colon] annotations in the arguments).
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
      that takes multiple arguments in first-order terms might take
      a product as only argument (see the following [product] function)
      in some languages (e.g. tptp), or be curryfied using this constructor
      in other languages (e.g. alt-ergo). *)

  val product : ?loc:location -> t -> t -> t
  (** Product type constructor, used for instance in the types of
      functions that takes multiple arguments in a non-curry way. *)

  val union   : ?loc:location -> t -> t -> t
  (** Union type constructor, currently used in tptp's THF format. *)

  val subtype : ?loc:location -> t -> t -> t
  (** Subtype relation for types. *)

  (** {3 Record constructors} *)

  val record : ?loc:location -> t list -> t
  (** Create a record expression. *)

  val record_with : ?loc:location -> t -> t list -> t
  (** Record "with" update (e.g. "{ r with ....}"). *)

  val record_access : ?loc:location -> t -> id -> t
  (** Field record access. *)

  (** {3 Algebraic datatypes} *)

  val adt_check : ?loc:location -> t -> id -> t
  (** Check whether some expression matches a given adt constructor
      (in head position). *)

  val adt_project : ?loc:location -> t -> id -> t
  (** Project a field of an adt constructor (usually unsafe except when
      guarded by an adt_check function). *)


  (** {3 Array constructors} *)

  val array_get : ?loc:location -> t -> t -> t
  (** Array getter. *)

  val array_set : ?loc:location -> t -> t -> t -> t
  (** Array setter. *)

  (** {3 Bitvector constructors} *)

  val bitv_extract : ?loc:location -> t -> int -> int -> t
  (** Bitvector extraction. *)

  val bitv_concat : ?loc:location -> t -> t -> t
  (** Bitvector concatenation. *)

  (** {3 Arithmetic constructors} *)

  val uminus : ?loc:location -> t -> t
  (** Arithmetic unary minus. *)

  val add    : ?loc:location -> t -> t -> t
  (** Arithmetic addition. *)

  val sub    : ?loc:location -> t -> t -> t
  (** Arithmetic substraction. *)

  val mult   : ?loc:location -> t -> t -> t
  (** Arithmetic multiplication. *)

  val div    : ?loc:location -> t -> t -> t
  (** Arithmetic division quotient. *)

  val mod_   : ?loc:location -> t -> t -> t
  (** Arithmetic modulo (aka division reminder). *)

  val int_pow : ?loc:location -> t -> t -> t
  (** Integer power. *)

  val real_pow : ?loc:location -> t -> t -> t
  (** Real power. *)

  val lt     : ?loc:location -> t -> t -> t
  (** Arithmetic "lesser than" comparison (strict). *)

  val leq    : ?loc:location -> t -> t -> t
  (** Arithmetic "lesser or equal" comparison. *)

  val gt     : ?loc:location -> t -> t -> t
  (** Arithmetic "greater than" comparison (strict). *)

  val geq    : ?loc:location -> t -> t -> t
  (** Arithmetic "greater or equal" comparison. *)

  (** {3 Triggers} *)

  val in_interval : ?loc:location -> t -> (t * bool) -> (t * bool) -> t
  (** Create a predicate for whether a term is within the given bounds
      (each bound is represented by a term which is tis value and a boolean
      which specifies whether it is strict or not). *)

  val maps_to : ?loc:location -> id -> t -> t
  (** Id mapping (see alt-ergo). *)

  val trigger : ?loc:location -> t list -> t
  (** Create a multi-trigger (i.e. all terms in the lsit must match to
      trigger). *)

  val triggers : ?loc:location -> t -> t list -> t
  (** [triggers ~loc f l] annotates formula/term [f] with a list of triggers. *)

  val filters : ?loc:location -> t -> t list -> t
  (** [filters ~loc f l] annotates formula/term [f] with a list of filters. *)


  (** {3 Special constructions} *)

  val tracked : ?loc:location -> id -> t -> t
  (** Name a term for tracking purposes. *)

  val quoted  : ?loc:location -> string -> t
  (** Create an attribute from a quoted string (in Zf). *)

  val sequent : ?loc:location -> t list -> t list -> t
  (** Sequents as terms *)

  val check   : ?loc:location -> t -> t
  (** Check a term (see alt-ergo). *)

  val cut     : ?loc:location -> t -> t
  (** Create a cut (see alt-ergo). *)

  val annot   : ?loc:location -> t -> t list -> t
  (** Attach a list of attributes (also called annotations) to a term. Attributes
      have no logical meaning (they can be safely ignored), but may serve to give
      hints or meta-information. *)

  val sexpr   : ?loc:location -> t list -> t
  (** S-expressions (for smtlib attributes), should probably be related
      to the [data_t] term. *)

end

(** {2 Signature for Response terms} *)

module type Response = Logic
(** Simply an alias to the Logic signature. *)


(** {2 Signature for Typechecked terms} *)

module type Tff = sig
  (** Signature required by terms for typing first-order
      polymorphic terms. *)

  type t
  (** The type of terms and term variables. *)

  type path
  (** The type of patsh to constants. *)

  type ty
  type ty_var
  type ty_const
  type ty_def
  (** The representation of term types, type variables, and type constants,
      and lastly type definitions. *)

  type 'a tag
  (** The type of tags used to annotate arbitrary terms. *)

  val ty : t -> ty
  (** Returns the type of a term. *)

  val print : Format.formatter -> t -> unit
  (** Printing function for terms. *)

  (** A module for variables that occur in terms. *)
  module Var : sig

    type t
    (** The type of variables the can occur in terms *)

    val print : Format.formatter -> t -> unit
    (** Printing function for term variables. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val mk : string -> ty -> t
    (** Create a new typed variable. *)

    val ty : t -> ty
    (** Return the type of the variable. *)

    val get_tag : t -> 'a tag -> 'a option
    (** Return the value bound to a tag (if any). *)

    val set_tag : t -> 'a tag -> 'a -> unit
    (** Set the value bound to the tag. *)

    val unset_tag : t -> _ tag -> unit
    (** Remove the binding to the given tag. *)

  end

  (** A module for constant symbols that occur in terms. *)
  module Const : sig

    type t
    (** The type of constant symbols that can occur in terms *)

    val print : Format.formatter -> t -> unit
    (** Printing function for term constants. *)

    val compare : t -> t -> int
    (** Comparison function on constant symbols. *)

    val ty : t -> ty
    (** Return the type of the constant. *)

    val mk : path -> ty -> t
    (** Create a constant symbol. *)

    val set_tag : t -> 'a tag -> 'a -> unit
    (** Tag a constant. *)

    val add_tag : t -> 'a list tag -> 'a -> unit
    (** Add a value to the list of values bound to a tag. *)

  end

  (** A module for Algebraic datatype constructors. *)
  module Cstr : sig

    type t
    (** An algebraic type constructor. Note that such constructors are used to
        build terms, and not types, e.g. consider the following:
        [type 'a list = Nil | Cons of 'a * 'a t], then [Nil] and [Cons] are the
        constructors, while [list] would be a type constant of arity 1 used to
        name the type. *)

    val ty : t -> ty
    (** Return the type of the constant. *)

    val compare : t -> t -> int
    (** Comparison function on constant symbols. *)

    val pattern_arity : t -> ty -> ty list -> ty list
    (** Used in the type-checking of pattern matching.
        [pattern_arity cstr ret ty_args] should return the types of the expected arguments
        [args] such that [apply_cstr cstr ty_args args] has type [ret]. *)

  end

  module Field : sig

    type t
    (** A field of a record. *)

    val compare : t -> t -> int
    (** Comparison function on constant symbols. *)

  end

  val define_adt :
    ty_const -> ty_var list ->
    (path * (ty * path option) list) list ->
    ty_def * (Cstr.t * (ty * Const.t option) list) list
  (** [define_aft t vars cstrs] defines the type constant [t], parametrised over
      the type variables [ty_vars] as defining an algebraic datatypes with constructors
      [cstrs]. [cstrs] is a list where each elements of the form [(name, l)] defines
      a new constructor for the algebraic datatype, with the given name. The list [l]
      defines the arguments to said constructor, each element of the list giving the
      type [ty] of the argument expected by the constructor (which may contain any of the type
      variables in [vars]), as well as an optional destructor name. If the construcotr name
      is [Some s], then the ADT definition also defines a function that acts as destructor
      for that particular field. This polymorphic function is expected to takes as arguments
      as many types as there are variables in [vars], an element of the algebraic datatype
      being defined, and returns a value for the given field.
      For instance, consider the following definition for polymorphic lists:
      [define_adt list \[ty_var_a\] \[
        "nil", \[\];
        "const", \[
          (Ty.of_var ty_var_a , Some "hd");
          (ty_list_a          , Some "tl");
          \];
       \]
      ]
      This definition defines the usual type of polymorphic linked lists, as well as two
      destructors "hd" and "tl". "hd" would have type [forall alpha. alpha list -> a], and
      be the partial function returning the head of the list.
  *)

  val define_record :
    ty_const -> ty_var list -> (path * ty) list -> ty_def * Field.t list
  (** Define a (previously abstract) type to be a record type, with the given fields. *)

  exception Wrong_type of t * ty
  (** Exception raised in case of typing error during term construction.
      [Wrong_type (t, ty)] should be raised by term constructor functions when some term [t]
      is expected to have type [ty], but does not have that type. *)

  exception Wrong_sum_type of Cstr.t * ty
  (** Raised when some constructor was expected to belong to some type but does not
      belong to the given type. *)

  exception Wrong_record_type of Field.t * ty_const
  (** Exception raised in case of typing error during term construction.
      This should be raised when the returned field was expected to be a field
      for the returned record type constant, but it was of another record type. *)

  exception Field_repeated of Field.t
  (** Field repeated in a record expression. *)

  exception Field_missing of Field.t
  (** Field missing in a record expression. *)

  exception Pattern_expected of t
  (** Raised when trying to create a pattern matching, but a non-pattern term
      was provided where a pattern was expected. *)

  exception Empty_pattern_matching
  (** Raise when creating a pattern matching but an empty list of branches
      was provided *)

  exception Partial_pattern_match of t list
  (** Raised when a partial pattern matching was created. A list of terms not
      covered by the patterns is provided. *)

  exception Over_application of t list
  (** Raised when an application was provided too many term arguments. The
      extraneous arguments are returned by the exception. *)

  exception Bad_poly_arity of ty_var list * ty list
  (** Raised when a polymorphic application does not have an
      adequate number of arguments. *)


  val ensure : t -> ty -> t
  (** Ensure that a given term has the given type. *)

  val of_var : Var.t -> t
  (** Create a term from a variable *)

  val apply_cst : Const.t -> ty list -> t list -> t
  (** Polymorphic application of a constant. *)

  val apply_cstr : Cstr.t -> ty list -> t list -> t
  (** Polymorphic application of a constructor. *)

  val apply_field : Field.t -> t -> t
  (** Apply a field to a record. *)

  val record : (Field.t * t) list -> t
  (** Create a record. *)

  val record_with : t -> (Field.t * t) list -> t
  (** Create an updated record *)

  val cstr_tester : Cstr.t -> t -> t
  (** Given a constructor [c] and a term [t], returns a terms that evaluates
      to [true] iff [t] has [c] as head constructor. *)

  val _and : t list -> t
  (** Conjunction of formulas *)


  val lam : ty_var list * Var.t list -> t -> t
  (** Create a local function. *)

  val all : ty_var list * Var.t list -> t -> t
  (** Universally quantify the given formula over the type and terms variables. *)

  val ex : ty_var list * Var.t list -> t -> t
  (** Existencially quantify the given formula over the type and terms variables. *)

  val bind : Var.t -> t -> t
  (** Bind a variable to an expressions. This function is called when typing
      a let-binding, before the body of the let-binding is typed. The returned
      expressions is used to replace the variable everywhere in the body of the
      let-binding being typed. *)

  val letin : (Var.t * t) list -> t -> t
  (** Create a sequential let-binding. *)

  val letand : (Var.t * t) list -> t -> t
  (** Create a parrallel let-binding. *)

  val pattern_match :
    ?redundant:(t -> unit) -> t -> (t * t) list -> t
  (** [pattern_match scrutinee branches] creates a pattern match expression
      on the scrutinee with the given branches, each of the form
      [(pattern, body)] *)

  val set_tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given formula wiht the tag and value. *)

  val add_tag : t -> 'a list tag -> 'a -> unit
  (** Add a value to the list of values bound to a tag. *)

  val fv : t -> ty_var list * Var.t list
  (** Returns the list of free variables in the formula. *)

end

module type Thf = sig

  include Tff

  val apply : t -> ty list -> t list -> t
  (** Polymorphic application. *)

end

(** Minimum required to type dimacs *)
module type Dimacs = sig

  type t
  (** The type of terms *)

  val neg : t -> t
  (** Logical negation. *)

end


(** Minimum required to type ae's tff *)
module type Ae_Base = sig

  type t
  (** The type of terms *)

  type term_var
  (** The type of term variables *)

  val void : t
  (** The only value of type unit. *)

  val eq : t -> t -> t
  (** Build the equality of two terms. *)

  val _true : t
  (** The smybol for [true] *)

  val _false : t
  (** The symbol for [false] *)

  val neg : t -> t
  (** Negation. *)

  val _or : t list -> t
  (** Disjunction of formulas *)

  val _and : t list -> t
  (** Conjunction of formulas *)

  val imply : t -> t -> t
  (** Implication *)

  val equiv : t -> t -> t
  (** Equivalence *)

  val xor : t -> t -> t
  (** Exclusive disjunction. *)

  val ite : t -> t -> t -> t
  (** [ite condition then_t else_t] creates a conditional branch. *)

  val distinct : t list -> t
  (** Distinct constraints on terms. *)

  val in_interval : t -> bool * bool -> t -> t -> t
  (** Semantic trigger: "in interval" check. *)

  val maps_to : term_var -> t -> t
  (** Semantic trigger: maps to. *)

end

module type Ae_Arith_Common = sig

  type t
  (** The type of terms *)

  val minus : t -> t
  (** Arithmetic unary minus/negation. *)

  val add : t -> t -> t
  (** Arithmetic addition. *)

  val sub : t -> t -> t
  (** Arithmetic substraction *)

  val mul : t -> t -> t
  (** Arithmetic multiplication *)

  val pow : t -> t -> t
  (** Arithmetic exponentiation *)

  val lt : t -> t -> t
  (** Arithmetic "less than" comparison. *)

  val le : t -> t -> t
  (** Arithmetic "less or equal" comparison. *)

  val gt : t -> t -> t
  (** Arithmetic "greater than" comparison. *)

  val ge : t -> t -> t
  (** Arithmetic "greater or equal" comparison. *)

end

(** Minimum required to type ae's arith *)
module type Ae_Arith = sig

  type t
  (** The type of terms. *)

  type ty
  (** The type of types. *)

  val ty : t -> ty
  (** Get the type of a term. *)

  val int : string -> t
  (** Integer literals *)

  val real : string -> t
  (** Real literals *)

  val _and : t list -> t
  (** Conjunction of formulas *)

  module Int : sig
    include Ae_Arith_Common with type t := t

    val div_e : t -> t -> t
    (** Euclidian division quotient *)

    val rem_e : t -> t -> t
    (** Euclidian division remainder *)

    val to_real : t -> t
    (** Conversion from an integer term to a real term. *)

  end

  module Real : sig
    include Ae_Arith_Common with type t := t

    val div : t -> t -> t
    (** Exact division on reals. *)

  end

end

module type Ae_Array = sig

  type t
  (** The type of terms *)

  val select : t -> t -> t
  (** [select arr idx] creates the get operation on functionnal
        array [arr] for index [idx]. *)

  val store : t -> t -> t -> t
  (** [store arr idx value] creates the set operation on
      functional array [arr] for value [value] at index [idx]. *)

end

(** Minimum required to type ae's bitvectors *)
module type Ae_Bitv = sig

  type t
  (** The type of terms *)

  val mk : string -> t
  (** Create a bitvector litteral from a string representation.
      The string should only contain characters '0' or '1'. *)

  val concat : t -> t -> t
  (** Bitvector concatenation. *)

  val extract : int -> int -> t -> t
  (** Bitvector extraction, using in that order,
      the start and then end positions of the
      bitvector to extract. *)

  val repeat : int -> t -> t
  (** Repetition of a bitvector. *)

  val zero_extend : int -> t -> t
  (** Extend the given bitvector with the given number of 0s. *)

  val sign_extend : int -> t -> t
  (** Extend the given bitvector with its most significant bit
      repeated the given number of times. *)

  val rotate_right : int -> t -> t
  (** [rotate_right i x] means rotate bits of x to the right i times. *)

  val rotate_left : int -> t -> t
  (** [rotate_left i x] means rotate bits of x to the left i times. *)

  val not : t -> t
  (** Bitwise negation. *)

  val and_ : t -> t -> t
  (** Bitwise conjunction. *)

  val or_ : t -> t -> t
  (** Bitwise disjunction. *)

  val nand : t -> t -> t
  (** [nand s t] abbreviates [not (and_ s t)]. *)

  val nor : t -> t -> t
  (** [nor s t] abbreviates [not (or_ s t)]. *)

  val xor : t -> t -> t
  (** [xor s t] abbreviates [or_ (and_ s (not t)) (and_ (not s) t)]. *)

  val xnor : t -> t -> t
  (** [xnor s t] abbreviates [or_ (and_ s t) (and_ (not s) (not t))]. *)

  val comp : t -> t -> t
  (** Bitwise comparison. [comp s t] equals [#b1] iff [s] and [t]
      are bitwise equal. *)

  val neg : t -> t
  (** Arithmetic complement on bitvectors.
      Supposing an input bitvector of size [m] representing
      an integer [k], the resulting term should represent
      the integer [2^m - k]. *)

  val add : t -> t -> t
  (** Arithmetic addition on bitvectors, modulo the size of
      the bitvectors (overflows wrap around [2^m] where [m]
      is the size of the two input bitvectors). *)

  val sub : t -> t -> t
  (** Arithmetic substraction on bitvectors, modulo the size
      of the bitvectors (2's complement subtraction modulo).
      [sub s t] should be equal to [add s (neg t)]. *)

  val mul : t -> t -> t
  (** Arithmetic multiplication on bitvectors, modulo the size
      of the bitvectors (see {!add}). *)

  val udiv : t -> t -> t
  (** Arithmetic euclidian integer division on bitvectors. *)

  val urem : t -> t -> t
  (** Arithmetic euclidian integer remainder on bitvectors. *)

  val sdiv : t -> t -> t
  (** Arithmetic 2's complement signed division.
      (see smtlib's specification for more information). *)

  val srem : t -> t -> t
  (** Arithmetic 2's complement signed remainder (sign follows dividend).
      (see smtlib's specification for more information). *)

  val smod : t -> t -> t
  (** Arithmetic 2's complement signed remainder (sign follows divisor).
      (see smtlib's specification for more information). *)

  val shl : t -> t -> t
  (** Logical shift left. [shl t k] return the result of
      shifting [t] to the left [k] times. In other words,
      this should return the bitvector representing
      [t * 2^k] (since bitvectors represent integers using
      the least significatn bit in cell 0). *)

  val lshr : t -> t -> t
  (** Logical shift right. [lshr t k] return the result of
      shifting [t] to the right [k] times. In other words,
      this should return the bitvector representing
      [t / (2^k)]. *)

  val ashr : t -> t -> t
  (** Arithmetic shift right, like logical shift right except that the most
      significant bits of the result always copy the most significant
      bit of the first argument*)

  val ult : t -> t -> t
  (** Boolean arithmetic comparison (less than).
      [ult s t] should return the [true] term iff [s < t]. *)

  val ule : t -> t -> t
  (** Boolean arithmetic comparison (less or equal than). *)

  val ugt : t -> t -> t
  (** Boolean arithmetic comparison (greater than). *)

  val uge : t -> t -> t
  (** Boolean arithmetic comparison (greater or equal than). *)

  val slt : t -> t -> t
  (** Boolean signed arithmetic comparison (less than).
      (See smtlib's specification for more information) *)

  val sle : t -> t -> t
  (** Boolean signed arithmetic comparison (less or equal than). *)

  val sgt : t -> t -> t
  (** Boolean signed arithmetic comparison (greater than). *)

  val sge : t -> t -> t
  (** Boolean signed arithmetic comparison (greater or equal than). *)

end

(** Minimum required to type tptp's tff *)
module type Tptp_Tff_Core = sig

  type t
  (** The type of terms *)

  val _true : t
  (** The smybol for [true] *)

  val _false : t
  (** The symbol for [false] *)

  val neg : t -> t
  (** Negation. *)

  val _or : t list -> t
  (** Disjunction of formulas *)

  val _and : t list -> t
  (** Conjunction of formulas *)

  val nand : t -> t -> t
  (** Not-and *)

  val nor : t -> t -> t
  (** Not-or *)

  val imply : t -> t -> t
  (** Implication *)

  val implied : t -> t -> t
  (** Implication *)

  val equiv : t -> t -> t
  (** Equivalence *)

  val xor : t -> t -> t
  (** Exclusive disjunction. *)

  val ite : t -> t -> t -> t
  (** [ite condition then_t else_t] creates a conditional branch. *)

  val eq : t -> t -> t
  (** Build the equality of two terms. *)

  val neq : t -> t -> t
  (** Disequality. *)

  val distinct : t list -> t
  (** Distinct constraints on terms. *)

end

module type Tptp_Thf_Core_Const = sig

  type t
  (** Type for term constans *)

  val _true : t
  (** The smybol for [true] *)

  val _false : t
  (** The symbol for [false] *)

  val neg : t
  (** Negation. *)

  val or_ : t
  (** Binary disjunction of formulas *)

  val and_ : t
  (** Binary conjunction of formulas *)

  val nand : t
  (** Not-and *)

  val nor : t
  (** Not-or *)

  val imply : t
  (** Implication *)

  val implied : t
  (** Reverse implication *)

  val equiv : t
  (** Equivalence *)

  val xor : t
  (** Exclusive disjunction. *)

  val ite : t
  (** [ite condition then_t else_t] creates a conditional branch. *)

  val eq : t
  (** Build the equality of two terms. *)

  val neq : t
  (** Binary disequality. *)

  val pi : t
  (** Higher-order encoding of universla quantification. *)

  val sigma : t
  (** Higher-order encoding of existancial quantification. *)

end

(** Minimum required to type tptp's thf *)
module type Tptp_Thf_Core = sig

  type t
  (** The type of terms *)

  type ty
  (** The type of types *)

  module Const : Tptp_Thf_Core_Const
  (** Constants *)

  val of_cst : Const.t -> t
  (** Create a term out of aconstant. *)

  val distinct : t list -> t
  (** Distinct constraints on terms. *)

end

(** Common signature for tptp arithmetics *)
module type Tptp_Tff_Arith_Common = sig

  type t
  (** The type of terms *)

  val minus : t -> t
  (** Arithmetic unary minus/negation. *)

  val add : t -> t -> t
  (** Arithmetic addition. *)

  val sub : t -> t -> t
  (** Arithmetic substraction *)

  val mul : t -> t -> t
  (** Arithmetic multiplication *)

  val div_e : t -> t -> t
  (** Euclidian division quotient *)

  val div_t : t -> t -> t
  (** Truncation of the rational/real division. *)

  val div_f : t -> t -> t
  (** Floor of the ration/real division. *)

  val rem_e : t -> t -> t
  (** Euclidian division remainder *)

  val rem_t : t -> t -> t
  (** Remainder for the truncation of the rational/real division. *)

  val rem_f : t -> t -> t
  (** Remaidner for the floor of the ration/real division. *)

  val lt : t -> t -> t
  (** Arithmetic "less than" comparison. *)

  val le : t -> t -> t
  (** Arithmetic "less or equal" comparison. *)

  val gt : t -> t -> t
  (** Arithmetic "greater than" comparison. *)

  val ge : t -> t -> t
  (** Arithmetic "greater or equal" comparison. *)

  val floor : t -> t
  (** Floor function. *)

  val ceiling : t -> t
  (** Ceiling *)

  val truncate : t -> t
  (** Truncation. *)

  val round : t -> t
  (** Rounding to the nearest integer. *)

  val is_int : t -> t
  (** Integer testing *)

  val is_rat : t -> t
  (** Rationality testing. *)

  val to_int : t -> t
  (** Convesion to an integer. *)

  val to_rat : t -> t
  (** Conversion to a rational. *)

  val to_real : t -> t
  (** Conversion to a real. *)

end

(** Signature required by terms for typing tptp arithmetic. *)
module type Tptp_Tff_Arith = sig

  type t
  (** The type of terms. *)

  type ty
  (** The type of types. *)

  val ty : t -> ty
  (** Get the type of a term. *)

  val int : string -> t
  (** Integer literals *)

  val rat : string -> t
  (** Rational literals *)

  val real : string -> t
  (** Real literals *)

  module Int : sig
    include Tptp_Tff_Arith_Common with type t := t
  end

  module Rat : sig
    include Tptp_Tff_Arith_Common with type t := t

    val div : t -> t -> t
    (** Exact division on rationals. *)
  end

  module Real : sig
    include Tptp_Tff_Arith_Common with type t := t

    val div : t -> t -> t
    (** Exact division on reals. *)
  end

end

(** Minimum required to type smtlib's core theory. *)
module type Smtlib_Base = sig

  type t
  (** The type of terms. *)

  type cstr
  (** The type of ADT constructor *)

  val _true : t
  (** The smybol for [true] *)

  val _false : t
  (** The symbol for [false] *)

  val neg : t -> t
  (** Negation. *)

  val _or : t list -> t
  (** Disjunction of formulas *)

  val _and : t list -> t
  (** Disjunction of formulas *)

  val nand : t -> t -> t
  (** Not-and *)

  val nor : t -> t -> t
  (** Not-or *)

  val imply : t -> t -> t
  (** Implication *)

  val equiv : t -> t -> t
  (** Equivalence *)

  val xor : t -> t -> t
  (** Exclusive disjunction. *)

  val ite : t -> t -> t -> t
  (** [ite condition then_t else_t] creates a conditional branch. *)

  val eq : t -> t -> t
  (** Create a chain of equalities. *)

  val distinct : t list -> t
  (** Distinct constraints on terms. *)

end

(** Common signature for first-order arithmetic *)
module type Smtlib_Arith_Common = sig

  type t
  (** The type of terms *)

  type cst
  (** The type of term constants. *)

  val mk : string -> t
  (** Build a constant. The literal is passed
      as a string to avoid overflow caused
      by the limited precision of native number formats. *)

  val minus : t -> t
  (** Arithmetic unary minus/negation. *)

  val add : t -> t -> t
  (** Arithmetic addition. *)

  val sub : t -> t -> t
  (** Arithmetic substraction *)

  val mul : t -> t -> t
  (** Arithmetic multiplication *)

  val lt : t -> t -> t
  (** Arithmetic "less than" comparison. *)

  val le : t -> t -> t
  (** Arithmetic "less or equal" comparison. *)

  val gt : t -> t -> t
  (** Arithmetic "greater than" comparison. *)

  val ge : t -> t -> t
  (** Arithmetic "greater or equal" comparison. *)

end



(** Signature required by terms for typing smtlib int arithmetic. *)
module type Smtlib_Int = sig

  include Smtlib_Arith_Common

  val div' : cst
  (** Constant for the division. *)

  val div : t -> t -> t
  (** Euclidian division. See Smtlib theory for a full description. *)

  val rem' : cst
  (** Constant for the remainder. *)

  val rem : t -> t -> t
  (** Euclidian integer remainder See Smtlib theory for a full description. *)

  val abs : t -> t
  (** Arithmetic absolute value. *)

  val divisible : string -> t -> t
  (** Arithmetic divisibility predicate. Indexed over
      constant integers (represented as strings, see {!int}). *)

end

(** Signature required by terms for typing smtlib real arithmetic. *)
module type Smtlib_Real = sig

  include Smtlib_Arith_Common

  val div' : cst
  (** Constant for the division. *)

  val div : t -> t -> t
  (** Real division. See Smtlib theory for a full description. *)

end

(** Signature required by terms for typing smtlib real_int arithmetic. *)
module type Smtlib_Real_Int = sig

  type t
  (** The type of terms. *)

  type ty
  (** The type of types. *)

  val ty : t -> ty
  (** Get the type of a term. *)

  module Int : sig

    include Smtlib_Int with type t := t

    val to_real : t -> t
    (** Conversion from an integer term to a real term. *)

  end

  module Real : sig

    include Smtlib_Real with type t := t

    val is_int : t -> t
    (** Arithmetic predicate, true on reals that are also integers. *)

    val floor_to_int : t -> t
    (** Greatest integer smaller than the given real *)

  end

end

module type Smtlib_Array = sig

  type t
  (** The type of terms *)

  type ty
  (** Type of of types. *)

  val const : ty -> t -> t
  (** [const ty base] creates a constant array that maps any value
      of type [ty] to [base]. *)

  val select : t -> t -> t
  (** [select arr idx] creates the get operation on functionnal
        array [arr] for index [idx]. *)

  val store : t -> t -> t -> t
  (** [store arr idx value] creates the set operation on
      functional array [arr] for value [value] at index [idx]. *)

end

module type Smtlib_Bitv = sig

  type t
  (** The type of terms *)

  val mk : string -> t
  (** Create a bitvector litteral from a string representation.
        The string should only contain characters '0' or '1'. *)

  val concat : t -> t -> t
  (** Bitvector concatenation. *)

  val extract : int -> int -> t -> t
  (** Bitvector extraction, using in that order,
      the end (exclusive) and then the start (inclusive)
      position of the bitvector to extract. *)

  val repeat : int -> t -> t
  (** Repetition of a bitvector. *)

  val zero_extend : int -> t -> t
  (** Extend the given bitvector with the given number of 0s. *)

  val sign_extend : int -> t -> t
  (** Extend the given bitvector with its most significant bit
      repeated the given number of times. *)

  val rotate_right : int -> t -> t
  (** [rotate_right i x] means rotate bits of x to the right i times. *)

  val rotate_left : int -> t -> t
  (** [rotate_left i x] means rotate bits of x to the left i times. *)

  val not : t -> t
  (** Bitwise negation. *)

  val and_ : t -> t -> t
  (** Bitwise conjunction. *)

  val or_ : t -> t -> t
  (** Bitwise disjunction. *)

  val nand : t -> t -> t
  (** [nand s t] abbreviates [not (and_ s t)]. *)

  val nor : t -> t -> t
  (** [nor s t] abbreviates [not (or_ s t)]. *)

  val xor : t -> t -> t
  (** [xor s t] abbreviates [or_ (and_ s (not t)) (and_ (not s) t)]. *)

  val xnor : t -> t -> t
  (** [xnor s t] abbreviates [or_ (and_ s t) (and_ (not s) (not t))]. *)

  val comp : t -> t -> t
  (** Bitwise comparison. [comp s t] equals [#b1] iff [s] and [t]
      are bitwise equal. *)

  val neg : t -> t
  (** Arithmetic complement on bitvectors.
      Supposing an input bitvector of size [m] representing
      an integer [k], the resulting term should represent
      the integer [2^m - k]. *)

  val add : t -> t -> t
  (** Arithmetic addition on bitvectors, modulo the size of
      the bitvectors (overflows wrap around [2^m] where [m]
      is the size of the two input bitvectors). *)

  val sub : t -> t -> t
  (** Arithmetic substraction on bitvectors, modulo the size
      of the bitvectors (2's complement subtraction modulo).
      [sub s t] should be equal to [add s (neg t)]. *)

  val mul : t -> t -> t
  (** Arithmetic multiplication on bitvectors, modulo the size
      of the bitvectors (see {!add}). *)

  val udiv : t -> t -> t
  (** Arithmetic euclidian integer division on bitvectors. *)

  val urem : t -> t -> t
  (** Arithmetic euclidian integer remainder on bitvectors. *)

  val sdiv : t -> t -> t
  (** Arithmetic 2's complement signed division.
      (see smtlib's specification for more information). *)

  val srem : t -> t -> t
  (** Arithmetic 2's complement signed remainder (sign follows dividend).
      (see smtlib's specification for more information). *)

  val smod : t -> t -> t
  (** Arithmetic 2's complement signed remainder (sign follows divisor).
      (see smtlib's specification for more information). *)

  val shl : t -> t -> t
  (** Logical shift left. [shl t k] return the result of
      shifting [t] to the left [k] times. In other words,
      this should return the bitvector representing
      [t * 2^k] (since bitvectors represent integers using
      the least significatn bit in cell 0). *)

  val lshr : t -> t -> t
  (** Logical shift right. [lshr t k] return the result of
      shifting [t] to the right [k] times. In other words,
      this should return the bitvector representing
      [t / (2^k)]. *)

  val ashr : t -> t -> t
  (** Arithmetic shift right, like logical shift right except that the most
      significant bits of the result always copy the most significant
      bit of the first argument*)

  val ult : t -> t -> t
  (** Boolean arithmetic comparison (less than).
      [ult s t] should return the [true] term iff [s < t]. *)

  val ule : t -> t -> t
  (** Boolean arithmetic comparison (less or equal than). *)

  val ugt : t -> t -> t
  (** Boolean arithmetic comparison (greater than). *)

  val uge : t -> t -> t
  (** Boolean arithmetic comparison (greater or equal than). *)

  val slt : t -> t -> t
  (** Boolean signed arithmetic comparison (less than).
      (See smtlib's specification for more information) *)

  val sle : t -> t -> t
  (** Boolean signed arithmetic comparison (less or equal than). *)

  val sgt : t -> t -> t
  (** Boolean signed arithmetic comparison (greater than). *)

  val sge : t -> t -> t
  (** Boolean signed arithmetic comparison (greater or equal than). *)

end

(** Bitvector part of the smtlib float requirements *)
module type Smtlib_Float_Bitv = sig

  type t
  (** the type of terms *)

  val mk : string -> t
  (** Bitvector litteral. *)

end

(** Real part of the smtlib float requirements *)
module type Smtlib_Float_Real = sig

  type t
  (** the type of terms *)

  val mk : string -> t
  (** Bitvector litteral. *)

end

(** Float part of the smtlib float requirements *)
module type Smtlib_Float_Float = sig

  type t
  (** the type of terms *)

  type cst
  (** The type of term constants. *)

  val fp : t -> t -> t -> t
  (** Construct a floating point from bitvector literals
      (sign, exponent, significand). The sign should be of size 1. *)

  val roundNearestTiesToEven: t
  (** constant for rounding mode RNE *)

  val roundNearestTiesToAway: t
  (** constant for rounding mode RNA *)

  val roundTowardPositive: t
  (** constant for rounding mode RTP *)

  val roundTowardNegative: t
  (** constant for rounding mode RTN *)

  val roundTowardZero: t
  (** constant for rounding mode RTZ *)

  val plus_infinity: int -> int -> t
  (** The constant plus infinity, it is also equivalent to a literal *)

  val minus_infinity: int -> int -> t
  (** The constant minus infinity, it is also equivalent to a literal *)

  val plus_zero: int -> int -> t
  (** The constant plus zero, it is also equivalent to a literal *)

  val minus_zero: int -> int -> t
  (** The constant minus zero, it is also equivalent to a literal *)

  val nan: int -> int -> t
  (** The constant Non-numbers, it is also equivalent to many literals which are
      equivalent together *)

  val abs : t -> t
  (** absolute value *)

  val neg : t -> t
  (** negation *)

  val add : t -> t -> t -> t
  (** [add rm f1 f2] addition *)

  val sub : t -> t -> t -> t
  (** [sub rm f1 f2] subtraction *)

  val mul : t -> t -> t -> t
  (** [mul rm f1 f2] multiplication *)

  val div : t -> t -> t -> t
  (** [mul rm f1 f2] division *)

  val fma : t -> t -> t -> t -> t
  (** [mul rm f1 f2] fused multiplication and addition *)

  val sqrt : t -> t -> t
  (** [sqrt rm f] square root *)

  val rem : t -> t -> t
  (** [rem f1 f2] remainder *)

  val roundToIntegral : t -> t -> t
  (** [roundToIntegral rm f] rounding to integral *)

  val min : t -> t -> t
  (** [min f1 f2] minimum *)

  val min' : int * int -> cst
  (** Constant for float min. *)

  val max : t -> t -> t
  (** [max f1 f2] maximum *)

  val max' : int * int -> cst
  (** Constant for float max. *)

  val leq : t -> t -> t
  (** [leq f1 f2] less or equal floating point comparison *)

  val lt : t -> t -> t
  (** [lt f1 f2] less than floating point comparison *)

  val geq : t -> t -> t
  (** [geq f1 f2] greater or equal floating point comparison *)

  val gt : t -> t -> t
  (** [lt f1 f2] greater than floating point comparison *)

  val eq : t -> t -> t
  (** [eq f1 f2] floating point equality *)

  val isNormal : t -> t
  (** [isNormal f] test if it is a normal floating point *)

  val isSubnormal : t -> t
  (** [isSubnormal f] test if it is a subnormal floating point *)

  val isZero : t -> t
  (** [isZero f] test if it is a zero *)

  val isInfinite : t -> t
  (** [isInfinite f] test if it is an infinite *)

  val isNaN : t -> t
  (** [isNaN f] test if it is NaN *)

  val isNegative : t -> t
  (** [isNegative f] test if it is a negative floating point *)

  val isPositive : t -> t
  (** [isPositive f] test if it is a positive floating point *)

  val ieee_format_to_fp: int -> int -> t -> t
  (** [ieee_format_to_fp e s bv] Convert a bitvector into a floating point using IEEE 754-2008 interchange format.
      (reinterpret the bitvector into floating-point)
  *)

  val to_fp: int -> int -> t -> t -> t
  (** [to_fp e s rm f] convert from one floating point format to another *)

  val real_to_fp: int -> int -> t -> t -> t
  (** [real_to_fp e s rm r] convert from a real *)

  val sbv_to_fp: int -> int -> t -> t -> t
  (** [sbv_to_fp e s rm bv] convert from a signed integer *)

  val ubv_to_fp: int -> int -> t -> t -> t
  (** [ubv_to_fp e s rm bv] convert from an unsigned integer *)

  val to_ubv: int -> t -> t -> t
  (** [to_ubv m rm f] convert to an unsigned integer (bitvector of size m) *)

  val to_ubv' : int -> int * int -> cst
  (** constant for [to_ubv] *)

  val to_sbv: int -> t -> t -> t
  (** [to_ubv m rm f] convert to a signed integer (bitvector of size m) *)

  val to_sbv' : int -> int * int -> cst
  (** constant for [to_sbv] *)

  val to_real: t -> t
  (** [to_real f] convert to a real *)

end

(* Requirement for the Floats SMTLIB theory *)
module type Smtlib_Float = sig

  (** Floating points are complicated so this documentation is not in anyway
      sufficient. A detailed description of the theory together with the
      rationale of several models decisions as well as a formal semantics of the
      theory can be found in

      [BTRW15] Martin Brain, Cesare Tinelli, Philipp Ruemmer, and Thomas Wahl. An
      Automatable Formal Semantics for IEEE-754 Floating-Point Arithmetic
      Technical Report, 2015. (http://smt-lib.org/papers/BTRW15.pdf)
  *)

  type t
  (** The type of terms *)

  type ty
  (** The type of types. *)

  type cst
  (** The type of term constants *)

  val ty : t -> ty
  (** Type of a term. *)

  module Real : Smtlib_Float_Real with type t := t
  (** Sub-module used for namespacing the real part
      of the theory requirements *)

  module Bitv : Smtlib_Float_Bitv with type t := t
  (** Sub-module used for namespacing the bitvector part
      of the theory requirements *)

  module Float : Smtlib_Float_Float with type t := t
                                     and type cst := cst
  (** Sub-module used for namespacing the floating number part
      of the theory requirements *)

end

module type Smtlib_String_RegLan = sig

  type t
  (** The type of terms *)

  val empty : t
  (** The empty regular language. *)

  val all : t
  (** The language that contains all strings *)

  val allchar : t
  (** The language that contains all strings of length 1 *)

  val of_string : t -> t
  (** Singleton language containing a single string. *)

  val range : t -> t -> t
  (** [range s1 s2] is the language containing all singleton strings
      (i.e. string of length 1) that are lexicographically beetween
      [s1] and [s2], **assuming [s1] and [s2] are singleton strings**.
      Else it is the empty language. *)

  val concat : t -> t -> t
  (** Language concatenation. *)

  val union : t -> t -> t
  (** Language union. *)

  val inter : t -> t -> t
  (** language intersection. *)

  val star : t -> t
  (** Kleene closure. *)

  val cross : t -> t
  (** Kleene cross. [cross e] abbreviates [concat e (star e)] *)

  val complement : t -> t
  (** Complement. *)

  val diff : t -> t -> t
  (** Difference *)

  val option : t -> t
  (** Option. [option e] abbreviates [union e (of_string "")] *)

  val power : int -> t -> t
  (** [power n e] is [n]-th power of [e]. *)

  val loop : int -> int -> t -> t
  (** Loop. See SMTLIb documentation. *)

end

module type Smtlib_String_String = sig

  type t
  (** The type of terms *)

  val of_ustring : string -> t
  (** Create a string from a unicode UTF-8 encoded string (with escape sequences
      already interpreted as unicode characters). *)

  val length : t -> t
  (** Length of a string expression. *)

  val at : t -> t -> t
  (** Get the char at the given position. *)

  val is_digit : t -> t
  (** Is the string a singleton string with a single digit character ? *)

  val to_code : t -> t
  (** Returns the code point of the single character of the string,
      or [(-1)] is the string is not a singleton. *)

  val of_code : t -> t
  (** Returns the singleton string whose only character is the given
      code point. *)

  val to_int : t -> t
  (** Evaluates the string as a decimal natural number, or [(-1)] if
      it's not possible. *)

  val of_int : t -> t
  (** Convert an int expression to a string in decimal representation. *)

  val concat : t -> t -> t
  (** String concatenation. *)

  val sub : t -> t -> t -> t
  (** Substring extraction. *)

  val index_of : t -> t -> t -> t
  (** Index of the first occurrence of the second string in
      first one, starting at the position of the third argument. *)

  val replace : t -> t -> t -> t
  (** Replace the first occurrence. *)

  val replace_all : t -> t -> t -> t
  (** Replace all occurrences. *)

  val replace_re : t -> t -> t -> t
  (** Replace the leftmost, shortest re ocurrence. *)

  val replace_re_all : t -> t -> t -> t
  (** Replace left-to-right, each shortest non empty re occurrence. *)

  val is_prefix : t -> t -> t
  (** First string is a prefix of the second one. *)

  val is_suffix : t -> t -> t
  (** First string is a suffix of the second one. *)

  val contains : t -> t -> t
  (** First string contains the second one. *)

  val lt : t -> t -> t
  (** Lexicographic strict ordering. *)

  val leq : t -> t -> t
  (** Lexicographic large ordering. *)

  val in_re : t -> t -> t
  (** String Regular languager membership *)

  module RegLan : Smtlib_String_RegLan with type t := t
  (** Sub-module used for namespacing for the regular language part
      of the theory requirements. *)

end

module type Smtlib_String = sig

  type t
  (** The type of terms *)

  module String : Smtlib_String_String with type t := t
  (** Sub-module used for namespacing for the string part
      of the theory requirements. *)

end

module type Zf_Base = sig

  type t
  (** The type of terms *)

  val _true : t
  (** The smybol for [true] *)

  val _false : t
  (** The symbol for [false] *)

  val neg : t -> t
  (** Negation. *)

  val _or : t list -> t
  (** Disjunction of formulas *)

  val _and : t list -> t
  (** Conjunction of formulas *)

  val imply : t -> t -> t
  (** Logical Implication. *)

  val equiv : t -> t -> t
  (** Logical Equivalence. *)

  val eq : t -> t -> t
  (** Build the equality of two terms. *)

  val neq : t -> t -> t
  (** Disequality. *)

  val ite : t -> t -> t -> t
  (** If-then-else *)

end

module type Zf_Arith = sig

  type t
  (** The type of terms *)

  val int : string -> t
  (** Integer literals *)

  module Int : sig
    val minus : t -> t
    (** Arithmetic unary minus/negation. *)

    val add : t -> t -> t
    (** Arithmetic addition. *)

    val sub : t -> t -> t
    (** Arithmetic substraction *)

    val mul : t -> t -> t
    (** Arithmetic multiplication *)

    val lt : t -> t -> t
    (** Arithmetic "less than" comparison. *)

    val le : t -> t -> t
    (** Arithmetic "less or equal" comparison. *)

    val gt : t -> t -> t
    (** Arithmetic "greater than" comparison. *)

    val ge : t -> t -> t
    (** Arithmetic "greater or equal" comparison. *)
  end

end
