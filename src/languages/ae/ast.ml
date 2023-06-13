
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type Id = sig

  type t
  (** The type of identifiers *)

  type namespace
  (** The type for namespaces. *)

  val var : namespace
  (** Used for type variables. *)

  val term : namespace
  (** Usual namespace, used for terms and propositions. *)

  val sort : namespace
  (** Usual namespace, used for types. *)

  val decl : namespace
  (** Names used to refer to top-level declarations. *)

  val track : namespace
  (** Namespace used to tag and identify sub-terms occuring in files. *)

  val mk : namespace -> string -> t
  (** Make an identifier *)

  val tracked : track:t -> namespace -> string -> t
  (** Make an identifier with an additional name. *)

end

module type Term = sig

  type t
  (** The type of terms. *)

  type id
  (** The type of identifiers *)

  type location
  (** The type of locations attached to terms. *)

  val prop    : ?loc:location -> unit -> t
  val bool    : ?loc:location -> unit -> t
  val ty_unit : ?loc:location -> unit -> t
  val ty_int  : ?loc:location -> unit -> t
  val ty_real : ?loc:location -> unit -> t
  val ty_bitv : ?loc:location -> int -> t
  (** Builtin types. *)

  val void    : ?loc:location -> unit -> t
  val true_   : ?loc:location -> unit -> t
  val false_  : ?loc:location -> unit -> t
  (** Builtin constants.  *)

  val not_  : ?loc:location -> t -> t
  val and_  : ?loc:location -> t list -> t
  val or_   : ?loc:location -> t list -> t
  val xor   : ?loc:location -> t -> t -> t
  val imply : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t
  (** Propositional builtins. *)

  val int : ?loc:location -> string -> t
  val real : ?loc:location -> string -> t
  val hexa : ?loc:location -> string -> t
  (** Numerical constant creation. *)

  val uminus    : ?loc:location -> t -> t
  val add       : ?loc:location -> t -> t -> t
  val sub       : ?loc:location -> t -> t -> t
  val mult      : ?loc:location -> t -> t -> t
  val div       : ?loc:location -> t -> t -> t
  val mod_      : ?loc:location -> t -> t -> t
  val int_pow   : ?loc:location -> t -> t -> t
  val real_pow  : ?loc:location -> t -> t -> t
  val lt        : ?loc:location -> t -> t -> t
  val leq       : ?loc:location -> t -> t -> t
  val gt        : ?loc:location -> t -> t -> t
  val geq       : ?loc:location -> t -> t -> t
  (** Arithmetic builtins. *)

  val eq        : ?loc:location -> t -> t -> t
  val neq       : ?loc:location -> t list -> t
  (** Equality and disequality. *)

  val array_get : ?loc:location -> t -> t -> t
  val array_set : ?loc:location -> t -> t -> t -> t
  (** Array primitives. *)

  val bitv : ?loc:location -> string -> t
  (** Bitvector litteral. *)

  val bitv_extract : ?loc:location -> t -> int -> int -> t
  (** Bitvoector extraction.
      TODO: document meaning of the itnegers indexes. *)

  val bitv_concat : ?loc:location -> t -> t -> t
  (** Bitvector concatenation. *)

  val const   : ?loc:location -> id -> t
  (** Constants, i.e non predefined symbols. This includes both constants
      defined by theories, defined locally in a problem, and also quantified variables. *)

  val colon   : ?loc:location -> t -> t -> t
  (** Juxtaposition of terms, used to annotate terms with their type. *)

  val apply : ?loc:location -> t -> t list -> t
  (** Application of terms (as well as types). *)

  val arrow : ?loc:location -> t -> t -> t
  (** Create a function type. *)

  val ite : ?loc:location -> t -> t -> t -> t
  (** Conditional terms. *)

  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t
  (** Universal and existential quantifications. *)

  val letin : ?loc:location -> t list -> t -> t
  (** Let-binding. *)

  val match_ : ?loc:location -> t -> (t * t) list -> t
  (** Pattern matching. The first term is the term to match,
      and each tuple in the list is a match case, which is a pair
      of a pattern and a match branch. *)

  val record : ?loc:location -> t list -> t
  (** Create a record expression, with a list of equalities of the form
      "label = expr". *)

  val record_with : ?loc:location -> t -> t list -> t
  (** Record update, of the form "s with [label = expr, ...]". *)

  val record_access : ?loc:location -> t -> id -> t
  (** Record access for the field given by the identifier. *)

  val adt_check : ?loc:location -> t -> id -> t
  (** Create a check agains the given adt constructor. *)

  val adt_project : ?loc:location -> t -> id -> t
  (** Create a projection for the given field of an adt constructor. *)

  val check : ?loc:location -> t -> t
  (** Create a term to "check" a formula.
      TODO: ask @iguernlala about this. *)

  val cut : ?loc:location -> t -> t
  (** Create a cut.
      TODO: ask @iguernlala about this. *)

  val in_interval : ?loc:location -> t -> (t * bool) -> (t * bool) -> t
  (** Create a trigger for the given term/variable being inside
      of a given interval, which is given as a lower bound, and an upper bound.
      Each bound contains an expression for the bound value, as well as a boolean
      indicating whether the bound is strict or not. *)

  val maps_to : ?loc:location -> id -> t -> t
  (** Used in trigger creation. *)

  val trigger : ?loc:location -> t list -> t
  (** Create a (multi) trigger. *)

  val triggers : ?loc:location -> t -> t list -> t
  (** Annotate a term (generally a quantified formula), with a list of triggers. *)

  val filters : ?loc:location -> t -> t list -> t
  (** Annotate a term (genrally a quantified formula) with a list of filters. *)

  val tracked : ?loc:location -> id -> t -> t
  (** Annotate a term with an id for tracking purposes. *)

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

  val logic : ?loc:location -> ac:bool -> id list -> term -> t
  (** Function declaration. *)

  val record_type : ?loc:location -> id -> term list -> (id * term) list -> t
  (** Record type definition. *)

  val fun_def : ?loc:location -> id -> term list -> term list -> term -> term -> t
  (** Function definition. *)

  val pred_def : ?loc:location -> id -> term list -> term list -> term -> t
  (** Predicate definition. *)

  val defs : ?loc:location -> ?attrs:term list -> t list -> t
  (** Pack a list of mutually recursive definitions into a single statement. *)

  val abstract_type : ?loc:location -> id -> term list -> t
  (** Create a new abstract type, quantified over the given type variables. *)

  val algebraic_type : ?loc:location -> id -> term list -> (id * term list) list -> t
  (** An algebraic datatype definition. *)

  val rec_types : ?loc:location -> t list -> t
  (** Pack a list of mutually recursive algebraic datatypes together. *)

  val axiom : ?loc:location -> id -> term -> t
  (** Create an axiom. *)

  val case_split : ?loc:location -> id -> term -> t
  (** Create a case split. *)

  val theory : ?loc:location -> id -> id -> t list -> t
  (** Create a theory, extending another, with the given list of declarations. *)

  val rewriting : ?loc:location -> id -> term list -> t
  (** Create a (set of ?) rewriting rule(s). *)

  val prove_goal : ?loc:location -> id -> term -> t
  (** Goal declaration. *)

  val prove_sat : ?loc:location -> name:id -> term list -> t
  (** Check-sat declaration. *)

end
