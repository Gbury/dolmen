
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Type definitions} *)
(*  ************************************************************************* *)


(** {3 Common definitions} *)

type hash = private int
type index = private int
type 'a tag = 'a Tag.t

type builtin = ..
(* Extensible variant type for builtin operations. Encodes in its type
   arguments the lengths of the expected ty and term arguments respectively. *)

type builtin += Base
(** The base builtin has unknown argument lengths. *)

type ttype = Type
(** The type of types. *)

type 'ty id = private {
  ty            : 'ty;
  name          : string;
  index         : index; (** unique *)
  builtin       : builtin;
  mutable tags  : Tag.map;
}
(** The type of identifiers. ['ty] is the type for representing the type of
    the id, ['ty] and ['t_n] are the lengths of arguments as described by
    the {builtin} type. *)

type ('ttype, 'ty) function_type = {
  fun_vars : 'ttype id list; (* prenex forall *)
  fun_args : 'ty list;
  fun_ret : 'ty;
}
(** The type for representing function types. *)


(** {3 Types} *)

type ty_var = ttype id
(** Abbreviation for type variables. *)

and ty_const = (unit, ttype) function_type id
(** Type symbols have the expected length of their argument encoded. *)

and ty_descr =
  | Var of ty_var             (** Type variables *)
  | App of ty_const * ty list (** Application *)
(** Type descriptions. *)

and ty = {
  descr : ty_descr;
  mutable hash : hash; (* lazy hash *)
  mutable tags : Tag.map;
}
(** Types, which wrap type description with a memoized hash and some tags. *)

(** {3 Terms and formulas} *)

type term_var = ty id
(** Term variables *)

and term_const = (ttype, ty) function_type id
(** Term symbols, which encode their expected type and term argument lists lengths. *)

and term_descr =
  | Var of term_var                                         (** Variables *)
  | App of term_const * ty list * term list                 (** Application *)
  | Binder of binder * term                                 (** Binders *)
(** Term descriptions. *)

and binder =
  | Exists of ty_var list * term_var list
  | Forall of ty_var list * term_var list
  | Letin  of (term_var * term) list (**)
(** Binders. *)

and term = {
  ty : ty;
  descr : term_descr;
  mutable hash : hash;
  mutable tags : Tag.map;
}
(** Term, which wrap term descriptions. *)

type formula = term
(** Alias for signature compatibility (with Dolmen_loop.Pipes.Make for instance). *)


(** {2 Exceptions} *)
(*  ************************************************************************* *)

exception Bad_ty_arity of ty_const * ty list
exception Bad_term_arity of term_const * ty list * term list
exception Type_already_defined of ty_const

exception Filter_failed_ty of string * ty
exception Filter_failed_term of string * term


(** {2 Builtins Tags} *)
(*  ************************************************************************* *)

module Tags : sig

  type 'a t = 'a tag
  (** Polymorphic tags *)

  include Dolmen_intf.Tag.Smtlib_Base with type 'a t := 'a t
  (** Satsify the Smtlib interface. *)

  include Dolmen_intf.Tag.Zf_Base with type 'a t := 'a t
  (** Satsify the Zf interface. *)

end

(** {2 Filters} *)
(*  ************************************************************************* *)

module Filter : sig

  val reset : unit -> unit
  (** Reset all filters. *)

  module Linear : sig

    val active : bool ref
    (** If [true], only linear terms may be created.
        Trying to create a non-linear term will raise
        a [Filter_failed_ty] or [Filter_failed_term]
        exception. *)

    val name : string
    (** Name of the filter for linear expressions. *)

  end

  module Quantifier : sig

    val allow : bool ref
    (** If [false], trying to build a quantified term
        (i.e. contianing a forall or exists), will raise
        a [Filter_failed_term] exception. *)

    val name : string
    (** Name of the filter for qunatifier-free expressions. *)

  end

end

(** {2 Printing} *)
(*  ************************************************************************* *)

module Print : sig

  type 'a t = Format.formatter -> 'a -> unit
  (** Alias for the type printing functions. *)

  val print_index : bool ref
  (** Determines whether to print the unique index of each identifier or not. *)

  val name : Pretty.name Tag.t
  (** The name tag is used for the printing of identifiers.
      When an identifier has an name tag, its value is used instead of the
      identifier intrinsic name. *)

  val pos : Pretty.pos Tag.t
  (** Positioning for pretty printing. If this tag is set, the printing functions
      will ignore type arguments (for readability).
      [Pretty.Infix] uses the identifier as a separator when printing th argument list
      [Pretty.Prefix] just ignore type arguments. *)


  val id : _ id t
  (** Printer for ids *)

  val ttype : ttype t
  (** Printer for ttype. *)

  val ty_var : ty_var t
  (** Printer to print type variables along with their types. *)

  val term_var : term_var t
  (** Printer to print term variables along with their types. *)

  val ty_const : ty_const t
  (** Printer to print type constants along with their types. *)

  val term_const : term_const t
  (** Printer to print term constants along with their types. *)

  val ty : ty t
  (** Printer for types. *)

  val term : term t
  (** Printer for terms. *)

end

(** {2 Substitutions} *)
(*  ************************************************************************* *)

module Subst : sig
  (** Module to handle substitutions *)

  type ('a, 'b) t
  (** The type of substitutions from values of type ['a] to values of type ['b]. *)

  val empty : ('a, 'b) t
  (** The empty substitution *)

  val is_empty : ('a, 'b) t -> bool
  (** Test wether a substitution is empty *)

  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  (** Iterates over the bindings of the substitution. *)

  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Maps the given function over bound values *)

  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  (** Fold over the elements *)

  val merge :
    ('a -> 'b option -> 'c option -> 'd option) ->
    ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
  (** Merge two substitutions *)

  val filter : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
  (** Filter bindings base on a predicate. *)

  val bindings : ('a, 'b) t -> ('a * 'b) list
  (** Returns the list of bindings ofa substitution. *)

  val exists : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
  (** Tests wether the predicate holds for at least one binding. *)

  val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
  (** Tests wether the predicate holds for all bindings. *)

  val hash : ('b -> int) -> ('a, 'b) t -> int
  val compare : ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  (** Comparison and hash functions, with a comparison/hash function on values as parameter *)

  val print :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) t -> unit
  (** Prints the substitution, using the given functions to print keys and values. *)

  val debug :
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a, 'b) t -> unit
  (** Prints the substitution, using the given functions to print keys and values,
      includign some debug info. *)

  val choose : ('a, 'b) t -> 'a * 'b
  (** Return one binding of the given substitution, or raise Not_found if the substitution is empty.*)

  (** {5 Concrete subtitutions } *)
  module type S = sig

    type 'a key
    (** Polymorphic type of keys for the a subtitution *)

    val get : 'a key -> ('a key, 'b) t -> 'b
    (** [get v subst] returns the value associated with [v] in [subst], if it exists.
        @raise Not_found if there is no binding for [v]. *)

    val mem : 'a key -> ('a key, 'b) t -> bool
    (** [get v subst] returns wether there is a value associated with [v] in [subst]. *)

    val bind : ('a key, 'b) t -> 'a key -> 'b -> ('a key, 'b) t
    (** [bind v t subst] returns the same substitution as [subst] with the additional binding from [v] to [t].
        Erases the previous binding of [v] if it exists. *)

    val remove : 'a key -> ('a key, 'b) t -> ('a key, 'b) t
    (** [remove v subst] returns the same substitution as [subst] except for [v] which is unbound in the returned substitution. *)

  end

  module Var : S with type 'a key = 'a id
end

(** {2 Types} *)
(*  ************************************************************************* *)

module Ty : sig

  (** {4 Usual definitions} *)

  type t = ty
  (** The type of types. *)

  type subst = (ty_var, ty) Subst.t
  (** The type of substitutions over types. *)

  type 'a tag = 'a Tag.t
  (** A type for tags to attach to arbitrary types. *)

  val hash : t -> int
  (** A hash function for types, should be suitable to create hashtables. *)

  val equal : t -> t -> bool
  (** An equality function on types. Should be compatible with the hash function. *)

  val compare : t -> t -> int
  (** Comparison function over types. Should be compativle with the equality function. *)

  val print : Format.formatter -> t -> unit
  (** Printing function. *)


  (** {4 Type structure definition} *)

  type adt_case = {
    cstr : term_const;
    dstrs : term_const option array;
  }
  (** One case of an algebraic datatype definition. *)

  type def =
    | Abstract
    | Adt of {
        ty : ty_const;
        record : bool;
        cstrs : adt_case list;
      } (** *)
  (** The various ways to define a type inside the solver. *)

  val define : ty_const -> def -> unit
  (** Register a type definition. *)

  val definition : ty_const -> def option
  (** Return the definition of a type (if it exists). *)


  (** {4 Variables and constants} *)

  (** A module for variables that occur in types. *)
  module Var : sig

    type t = ty_var
    (** The type of variables the can occur in types *)

    val hash : t -> int
    (** A hash function for type variables, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on type variables. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val mk : string -> t
    (** Create a new type variable with the given name. *)

    val tag : t -> 'a tag -> 'a -> unit
    (** Tag a variable. *)

    val get_tag : t -> 'a tag -> 'a option
    (** Return the value associated to the tag (if any). *)

  end

  (** A module for constant symbols the occur in types. *)
  module Const : sig

    type t = ty_const
    (** The type of constant symbols the can occur in types *)

    val hash : t -> int
    (** A hash function for type constants, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on type constants. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val arity : t -> int
    (** Return the arity of the given symbol. *)

    val mk : string -> int -> t
    (** Create a type constant with the given arity. *)

    val tag : t -> 'a tag -> 'a -> unit
    (** Tag a variable. *)

    val get_tag : t -> 'a tag -> 'a option
    (** Return the value associated to the tag (if any). *)

    val int : t
    (** The type constant for integers *)

    val rat : t
    (** The type constant for rationals *)

    val real : t
    (** The type constant for reals. *)

    val prop : t
    (** The type constant for propositions *)

    val base : t
    (** An arbitrary type constant. *)

    val array : t
    (** The type constant for arrays *)

    val bitv : int -> t
    (** Bitvectors of the given length. *)

  end

  val prop : t
  (** The type of propositions *)

  val base : t
  (** An arbitrary type. *)

  val int : t
  (** The type of integers *)

  val rat : t
  (** The type of rationals *)

  val real : t
  (** The type of reals. *)

  val wildcard : unit -> t
  (** Type wildcard *)

  val of_var : Var.t -> t
  (** Create a type from a variable. *)

  val apply : Const.t -> t list -> t
  (** Application for types. *)

  val array : t -> t -> t
  (** Build an array type from source to destination types. *)

  val bitv : int -> t
  (** Bitvectors of a given length. *)

  val tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given type with the given tag and value. *)

  val get_tag : t -> 'a tag -> 'a option
  (** Return the value associated to the tag (if any). *)

  val subst : ?fix:bool -> subst -> t -> t
  (** Substitution on types. *)

end

(** {2 Terms} *)
(*  ************************************************************************* *)

module Term : sig

  (** Signature required by terms for typing first-order
      polymorphic terms. *)

  type t = term
  (** The type of terms and term variables. *)

  type ty = Ty.t
  type ty_var = Ty.Var.t
  type ty_const = Ty.Const.t
  (** The representation of term types, type variables, and type constants. *)

  type subst = (term_var, term) Subst.t
  (** The type of substitutions over terms. *)

  type 'a tag = 'a Tag.t
  (** The type of tags used to annotate arbitrary terms. *)

  val hash : t -> int
  (** Hash function. *)

  val equal : t -> t -> bool
  (** Equality function. *)

  val compare : t -> t -> int
  (** Comparison function. *)

  val print : Format.formatter -> t -> unit
  (** Printing function. *)

  val ty : t -> ty
  (** Returns the type of a term. *)

  (** A module for variables that occur in terms. *)
  module Var : sig

    type t = term_var
    (** The type of variables the can occur in terms *)

    val hash : t -> int
    (** A hash function for term variables, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on term variables. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val mk : string -> ty -> t
    (** Create a new typed variable. *)

    val ty : t -> ty
    (** Return the type of the variable. *)

    val tag : t -> 'a tag -> 'a -> unit
    (** Tag a variable. *)

    val get_tag : t -> 'a tag -> 'a option
    (** Return the value associated to the tag (if any). *)

  end

  (** A module for constant symbols that occur in terms. *)
  module Const : sig

    type t = term_const
    (** The type of constant symbols that can occur in terms *)

    val hash : t -> int
    (** A hash function for term constants, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on term constants. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val arity : t -> int * int
    (** Returns the arity of a term constant. *)

    val mk : string -> ty_var list -> ty list -> ty -> t
    (** Create a polymorphic constant symbol. *)

    val tag : t -> 'a tag -> 'a -> unit
    (** Tag a constant. *)

    val get_tag : t -> 'a tag -> 'a option
    (** Return the value associated to the tag (if any). *)

  end

  (** A module for Algebraic datatype constructors. *)
  module Cstr : sig

    type t = term_const
    (** An algebraic type constructor. Note that such constructors are used to
        build terms, and not types, e.g. consider the following:
        [type 'a list = Nil | Cons of 'a * 'a t], then [Nil] and [Cons] are the
        constructors, while [list] would be a type constant of arity 1 used to
        name the type. *)

    val hash : t -> int
    (** A hash function for adt constructors, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on adt constructors. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val arity : t -> int * int
    (** Returns the arity of a constructor. *)

    val tag : t -> 'a tag -> 'a -> unit
    (** Tag a constant. *)

    val get_tag : t -> 'a tag -> 'a option
    (** Return the value associated to the tag (if any). *)

  end

  (** A module for Record fields. *)
  module Field : sig

    type t = term_const
    (** A record field. *)

    val hash : t -> int
    (** A hash function for adt constructors, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on adt constructors. Should be compatible with the hash function. *)

  end

  val define_record :
    ty_const -> ty_var list -> (string * ty) list -> Field.t list
  (** Define a new record type. *)

  val define_adt :
    ty_const -> ty_var list ->
    (string * (ty * string option) list) list ->
    (Cstr.t * (ty * Const.t option) list) list
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

  exception Wrong_type of t * ty
  (** Exception raised in case of typing error during term construction.
      [Wrong_type (t, ty)] should be raised by term constructor functions when some term [t]
      is expected to have type [ty], but does not have that type. *)

  exception Wrong_record_type of Field.t * ty_const
  (** Exception raised in case of typing error during term construction.
      This should be raised when the returned field was expected to be a field
      for the returned record type constant, but it was of another record type. *)

  exception Field_repeated of Field.t
  (** Field repeated in a record expression. *)

  exception Field_missing of Field.t
  (** Field missing in a record expression. *)

  exception Field_expected of term_const
  (** A field was expected but the returned term constant is not a record field. *)

  val ensure : t -> ty -> t
  (** Ensure a term has the given type. *)

  val of_var : Var.t -> t
  (** Create a term from a variable *)

  val apply : Const.t -> ty list -> t list -> t
  (** Polymorphic application. *)

  val apply_cstr : Cstr.t -> ty list -> t list -> t
  (** Polymorphic application of a constructor. *)

  val apply_field : Field.t -> t -> t
  (** Field access for a record. *)

  val _true : t
  val _false : t
  (** Some usual formulas. *)

  val int : string -> t
  (* Integer literals *)

  val rat : string -> t
  (* Rational literals *)

  val real : string -> t
  (** Real literals *)

  val record : (Field.t * t) list -> t
  (** Create a record *)

  val record_with : t -> (Field.t * t) list -> t
  (** Record udpate *)

  val eq : t -> t -> t
  (** Build the equality of two terms. *)

  val eqs : t list -> t
  (** Build equalities with arbitrary arities. *)

  val distinct : t list -> t
  (** Distinct constraints on terms. *)

  val neg : t -> t
  (** Negation. *)

  val _and : t list -> t
  (** Conjunction of formulas *)

  val _or : t list -> t
  (** Disjunction of formulas *)

  val nand : t -> t -> t
  (** Negated conjunction. *)

  val nor : t -> t -> t
  (** Negated disjunction. *)

  val xor : t -> t -> t
  (** Exclusive disjunction. *)

  val imply : t -> t -> t
  (** Implication *)

  val equiv : t -> t -> t
  (** Equivalence *)

  val select : t -> t -> t
  (** Array selection. *)

  val store : t -> t -> t -> t
  (** Array store *)

  val all :
    ty_var list * Var.t list ->
    ty_var list * Var.t list ->
    t -> t
  (** Universally quantify the given formula over the type and terms variables.
      The first pair of arguments are the variables that are free in the resulting
      quantified formula, and the second pair are the variables bound. *)

  val ex :
    ty_var list * Var.t list ->
    ty_var list * Var.t list ->
    t -> t
  (** Existencially quantify the given formula over the type and terms variables.
      The first pair of arguments are the variables that are free in the resulting
      quantified formula, and the second pair are the variables bound. *)

  val letin : (Var.t * t) list -> t -> t
  (** Let-binding. Variabels can be bound to either terms or formulas. *)

  val ite : t -> t -> t -> t
  (** [ite condition then_t else_t] creates a conditional branch. *)

  val tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given formula wiht the tag and value. *)

  val get_tag : t -> 'a tag -> 'a option
  (** Return the value associated to the tag (if any). *)

  val fv : t -> ty_var list * Var.t list
  (** Returns the list of free variables in the formula. *)

  val subst : ?fix:bool -> Ty.subst -> subst -> t -> t
  (** Substitution over terms. *)

  include Dolmen_intf.Term.Smtlib_Bitv with type t := t
  (** Satisfy the required interface for typing smtlib bitvectors. *)

  (** Integer operations. *)
  module Int : sig
    include Dolmen_intf.Term.Smtlib_Int with type t := t
    include Dolmen_intf.Term.Tptp_Arith_Common with type t := t

    val div : t -> t -> t
    (** Euclidian division quotient *)

    val div_t : t -> t -> t
    (** Truncation of the rational/real division. *)

    val div_f : t -> t -> t
    (** Floor of the ration/real division. *)

    val rem : t -> t -> t
    (** Euclidian division remainder *)

    val rem_t : t -> t -> t
    (** Remainder for the truncation of the rational/real division. *)

    val rem_f : t -> t -> t
    (** Remaidner for the floor of the ration/real division. *)

  end

  (** Rational operations *)
  module Rat : sig
    include Dolmen_intf.Term.Tptp_Arith_Common with type t := t

    val div : t -> t -> t
    (** Exact division on rationals. *)
  end

  (** Real operations *)
  module Real : sig
    include Dolmen_intf.Term.Smtlib_Real with type t := t
    include Dolmen_intf.Term.Tptp_Arith_Common with type t := t
  end

end

