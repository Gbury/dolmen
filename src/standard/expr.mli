
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Temporary workaround for the fact that some of the module types included
   to simplify the documentation of the Term module ovarlap (i.e. multiple
   included signatures define the same function). *)
[@@@warning "-32"]

(** {2 Type definitions} *)
(*  ************************************************************************* *)


(** {3 Type definitions} *)

type hash = private int
type index = private int
type 'a tag = 'a Tag.t

type builtin = (var, cst, term) Builtin.t
(** Builtins *)

and pattern = term
(** Convenient alias to distinguish intent. *)

and var = private {
  var_index     : index; (** unique *)
  var_ty        : term;
  var_name      : string;
  var_builtin   : builtin;
  mutable var_tags : Tag.map;
}
(** Variables *)

and cst = private {
  cst_index     : index; (** unique *)
  cst_ty        : term;
  cst_name      : string;
  cst_builtin   : builtin;
  mutable cst_tags : Tag.map;
}
(** Constants *)

and binder = private
  | Pi of var list
  | Arrow of term list
  | Exists of var list
  | Forall of var list
  | Letin of (var * term) list (**)
(** Binders. *)

and descr =
  | Var of var
  | Cst of cst
  | App of term * term list
  | Binder of binder * term
  | Match of term * (pattern * term) list (**)
(** Term descriptions. *)

and term = private {
  ty : term;
  mutable descr : descr;
  mutable hash : hash;
  mutable tags : Tag.map;
}
(** Term, which wrap term descriptions. *)

type adt_case = {
  cstr : cst;
  (** Constructor for the ADT case. *)
  tester : cst;
  (** Tester for the ADT case. *)
  dstrs : cst option array;
  (** Optional destructors for the fields of the constructor. *)
}
(** One case of an algebraic datatype definition. *)

type def =
  | Abstract
  (** The type is abstract (this is the default for most types) *)
  | Adt of {
      ty : cst;
      record : bool;
      cases : adt_case array;
    }
  (** The type is an ADT, with a list of constructors.
      Note that records are seen as ADTs with only one constructor. *)
(** The various ways to define a type inside the solver. *)

type ty = term
type formula = term
(** Alias for signature compatibility (with Dolmen_loop.Pipes.Make for instance). *)


(** {2 Exceptions} *)
(*  ************************************************************************* *)

exception Kind_has_no_type
(** Exception raised when trying to access the type of [kind] through the
    {!Term.ty} function. To be theoretically sound, there should be a
    hierarchy of kinds, but that is not needed for typical automated
    reasoning, thus a bounded hierarchy is implemented, which contains:
    - terms (e.g. `1`, `f (1 + 2) (g b)`, ...)
    - types, (e.g. `int`, `int -> a -> a list`, ...)
    - Type, the type of types
    - Kind, the type of Type; however, Kind does not have a type, or
      rather it is an error to try and access it as it has no
      representation in the current implementation. *)

exception Type_already_defined of cst
(** Exception raised whan a type constructor is defined a second time. *)

exception Record_type_expected of cst
(** Exception raised when the provided type constructor was expected to
    be that of a record type, but that is not the case. *)

exception Bad_arity of term * term list
(** Exception raised in the case of over-applicaiton under-applications
    when creating a first-order application. *)

exception Expected_function_type of term
(** The given term was expect to have a functional type (typically because
    it was applied to some arguments), but that is not the case, hence an
    error (typically because the type of the resulting application cannot
    be computed). *)

exception Wrong_type of { term : term; expected_ty: term; }
(** Exception raised in case of typing error during term construction.
    It should be raised by term constructor functions when some term [term]
    is expected to have type [expected_ty], but does not have that type. *)

exception Field_missing of cst
(** Field missing in a record expression. *)

exception Field_repeated of cst
(** Field repeated in a record expression. *)

exception Field_expected of cst
(** A field was expected but the returned term constant is not a record field. *)

exception Constructor_expected of cst
(** A constructor was expected but the returned term constant is not a ADT constructor. *)

exception Wrong_sum_type of { cst : cst; expected_ty: term; }
(** Raised when some constant/constructor was expected to belong to some
    ADT, but does not belong to the given type. *)

exception Wrong_record_type of { cst : cst; record_ty: cst; }
(** Exception raised in case of typing error during term construction.
    This should be raised when the returned field was expected to be a field
    for the returned record type constant, but it was of another record type. *)


(** {2 Native Tags} *)
(*  ************************************************************************* *)

module Tags : sig

  type 'a t = 'a tag
  (** Polymorphic tags *)

  include Dolmen_intf.Tag.Ae_Base with type 'a t := 'a t
  (** Satsify the Ae interface. *)

  include Dolmen_intf.Tag.Smtlib_Base with type 'a t := 'a t
                                       and type term := term
  (** Satsify the Smtlib interface. *)

  include Dolmen_intf.Tag.Zf_Base with type 'a t := 'a t
  (** Satsify the Zf interface. *)

end

(** {2 Printing} *)
(*  ************************************************************************* *)

module Print : sig

  type 'a t = Format.formatter -> 'a -> unit
  (** Alias for the type printing functions. *)

  val print_index : bool ref
  (** Determines whether to print the unique index of each identifier or not. *)

  val pos : Pretty.pos Tag.t
  (** Positioning for pretty printing. If this tag is set, the printing functions
      will ignore type arguments (for readability).
      [Pretty.Infix] uses the identifier as a separator when printing th argument list
      [Pretty.Prefix] just ignore type arguments. *)

  val name : Pretty.name Tag.t
  (** The name tag is used for the printing of identifiers.
      When an identifier has an name tag, its value is used instead of the
      identifier intrinsic name. *)

  val var : var t
  (** Printer for variables *)

  val cst : cst t
  (** Printer for constants *)

  val cst_pretty : cst t
  (** Printer for constants, also prints the pretty printing information
      (i.e. the infix/prefix symbol if it exists). *)

  val term : term t
  (** Printer for terms. *)

end

(** {2 Views} *)
(*  ************************************************************************* *)

module View : sig

  module Ty : Dolmen_intf.View.Ty.S
    with type var := var
     and type cst := cst
     and type blt := builtin
     and type ty := ty

  module FO : Dolmen_intf.View.FO.S
    with type ty_var := var
     and type ty_cst := cst
     and type term_var := var
     and type term_cst := cst
     and type builtin := builtin
     and type ty := ty
     and type term := term

  module HO : Dolmen_intf.View.HO.S
    with type var := var
     and type cst := cst
     and type blt := builtin
     and type term := term

end

(** {2 Variables} *)
(*  ************************************************************************* *)

module Var : sig

  type t = var
  (** The type of identifiers *)

  val hash : t -> int
  (** Hash function. *)

  val equal : t -> t -> bool
  (** Equality function. *)

  val compare : t -> t -> int
  (** Comparison function. *)

  val print : Format.formatter -> t -> unit
  (** Printing function *)

  module Set : Set.S with type elt = t
  (** Sets of variables *)

  module Map : Map.S with type key = t
  (** Maps of variables *)

  module H : Hashtbl.S with type key = t
  (** Hashtables of variables *)

  val tag : t -> 'a Tag.t -> 'a -> unit
  (** Add a tag to an identifier *)

  val get_tag : t -> 'a Tag.t -> 'a list
  (** Get all the tags added to the identifier *)

  val get_tag_last : t -> 'a Tag.t -> 'a option
  (** Get the last tag added to the identifier *)

  val ty : t -> ty
  (** Type of a variable *)

  val make :
    ?pos:Pretty.pos ->
    ?name:string ->
    ?builtin:builtin ->
    ?tags:Tag.map ->
    string -> ty -> t
  (** Create a fresh identifer *)

  val mk : string -> ty -> t
  (** Shorthand for {make} without the optional arguments. *)

end

(** {2 Constants} *)
(*  ************************************************************************* *)

module Cst : sig

  type t = cst
  (** The type of identifiers *)

  val hash : t -> int
  (** Hash function. *)

  val equal : t -> t -> bool
  (** Equality function. *)

  val compare : t -> t -> int
  (** Comparison function. *)

  val print : Format.formatter -> t -> unit
  (** Printing function *)

  module Set : Set.S with type elt = t
  (** Sets of constants *)

  module Map : Map.S with type key = t
  (** Maps of constants *)

  module H : Hashtbl.S with type key = t
  (** Hashtables of constants *)

  val tag : t -> 'a Tag.t -> 'a -> unit
  (** Add a tag to an identifier *)

  val get_tag : t -> 'a Tag.t -> 'a list
  (** Get all the tags added to the identifier *)

  val get_tag_last : t -> 'a Tag.t -> 'a option
  (** Get the last tag added to the identifier *)

  val make :
    ?pos:Pretty.pos ->
    ?name:string ->
    ?builtin:builtin ->
    ?tags:Tag.map ->
    string -> ty -> t
  (** Create a fresh identifer *)

  val mk : string -> ty -> t
  (** Shorthand for {make} without the optional arguments. *)

end

(** {2 Constructors} *)
(*  ************************************************************************* *)

(** A module for Algebraic datatype constructors. *)
module Cstr : sig

  type t = cst
  (** An algebraic type constructor. Note that such constructors are used to
        build terms, and not types, e.g. consider the following:
        [type 'a list = Nil | Cons of 'a * 'a t], then [Nil] and [Cons] are the
        constructors, while [list] would be a type constant of arity 1 used to
        name the type. *)

  val hash : t -> int
  (** A hash function for adt constructors. *)

  val equal : t -> t -> bool
  (** An equality function on adt constructors. *)

  val compare : t -> t -> int
  (** Comparison function on variables. *)

  val print : Format.formatter -> t -> unit
  (** Printing function. *)

  module Set : Set.S with type elt = t
  (** Sets of constructors *)

  module Map : Map.S with type key = t
  (** Maps of constructors *)

  module H : Hashtbl.S with type key = t
  (** Hashtables of constructors *)

  val tag : t -> 'a tag -> 'a -> unit
  (** Tag a constant. *)

  val get_tag : t -> 'a tag -> 'a list
  (** Return the list of values associated to the tag. *)

  val get_tag_last : t -> 'a tag -> 'a option
  (** Return the last value associated to the tag (if any). *)

  val void : t
  (** Only constructor for the type unit. *)

  val apply : t -> term list -> term
  (** Apply the constructor to a list of terms. Type arguments are required
      if the constructor is polymorphic. *)

  val pattern_arity : t -> ty -> ty list -> ty list
  (** Used in the type-checking of pattern matching.
      [pattern_arity cstr ret ty_args] should return the types of the expected arguments
      [args] such that [apply_cstr cstr ty_args args] has type [ret].
      @raise Wrong_sum_type if [ret] cannot be unified with the type of [c]
      @raise Bad_term_arity if the provided type argument list is not of the correct length *)

  val tester : t -> cst
  (** Returns the tester associated to the given constructor. *)

  val test : t -> term -> term
  (** Get the tester for the given constructor and apply it to the given term
      (automatically filling in the necessary type arguments as required). *)

end

(** {2 Record fields} *)
(*  ************************************************************************* *)

(** A module for Record fields. *)
module Field : sig

  type t = cst
  (** A record field. *)

  val hash : t -> int
  (** A hash function for adt destructors. *)

  val equal : t -> t -> bool
  (** An equality function on adt destructors. *)

  val compare : t -> t -> int
  (** A comparison function on adt constructors. *)

  val print : Format.formatter -> t -> unit
  (** Printing function. *)

  module Set : Set.S with type elt = t
  (** Sets of fields *)

  module Map : Map.S with type key = t
  (** Maps of fields *)

  module H : Hashtbl.S with type key = t
  (** Hashtables of fields *)

  val apply : t -> term -> term
  (** Apply a field to a record (automatically fills in
      the type arguments as needed). *)

end

(** {2 Types} *)
(*  ************************************************************************* *)

module Ty : sig

  (** {4 Usual definitions} *)

  type t = ty
  (** The type of types. *)

  type 'a tag = 'a Tag.t
  (** A type for tags to attach to arbitrary types. *)

  val hash : t -> int
  (** A hash function for types. *)

  val equal : t -> t -> bool
  (** An equality function on types. *)

  val compare : t -> t -> int
  (** Comparison function over types. *)

  val print : Format.formatter -> t -> unit
  (** Printing function. *)


  (** {4 Tags} *)

  val tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given type with the given tag and value. *)

  val get_tag : t -> 'a tag -> 'a list
  (** Return the list of value associated to the tag. *)

  val get_tag_last : t -> 'a tag -> 'a option
  (** Return the last value associated to the tag (if any). *)


  (** {4 Type creation} *)

  val wildcard : unit -> t
  (** Type wildcard *)

  val of_var : var -> t
  (** Create a type from a variable. *)

  val of_cst : cst -> t
  (** Create a type from a variable. *)

  val apply : t -> t list -> t
  (** Application for types. *)


  (** {4 Free variables and substitution} *)

  val fv : t -> Var.Set.t
  (** Returns the list of free variables in the formula. *)

  val subst : ?fix:bool -> t Var.Map.t -> t -> t
  (** Substitution on types. *)


  (** {4 View} *)

  type view = (var, cst, builtin, ty) Dolmen_intf.View.Ty.t
  (** View on types. *)

  val view : t -> view
  (** View on types. *)


  (** {4 Type structure definition} *)

  val define : cst -> def -> unit
  (** Register a type definition. *)

  val definition : cst -> def option
  (** Return the definition of a type (if it exists). *)

  val define_record :
    cst -> var list -> (string * ty) list -> Field.t list
  (** [define_record t vars fields] defines a new record type
      from the type constructor [t], parametrized by the type variables
      [vars] (this should match the arity of [t]), and with the given list
      of fields (for each field, the name and type of the field is given). *)

  val define_adt :
    cst -> var list ->
    (string * (ty * string option) list) list ->
    (Cstr.t * (ty * Field.t option) list) list
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


  (** {4 Builtin types} *)

  val prop : t
  (** The type of propositions *)

  val bool : t
  (** Alias for {prop}. *)

  val unit : t
  (** The unit type. *)

  val base : t
  (** An arbitrary type. *)

  val int : t
  (** The type of integers *)

  val rat : t
  (** The type of rationals *)

  val real : t
  (** The type of reals. *)

  val string : t
  (** The type of strings *)

  val string_reg_lang : t
  (** The type of regular language over strings. *)

  val array : t -> t -> t
  (** Build an array type from source to destination types. *)

  val bitv : int -> t
  (** Bitvectors of a given length. *)

  val float : int -> int -> t
  (** Floating point of given exponent and significand. *)

  val roundingMode : t
  (** Type for the various rounding modes for float operations. *)


  (** {4 Builtin type constants} *)

  (** A module for constant symbols the occur in types. *)
  module Cst : sig

    val int : cst
    (** The type constant for integers *)

    val rat : cst
    (** The type constant for rationals *)

    val real : cst
    (** The type constant for reals. *)

    val prop : cst
    (** The type constant for propositions *)

    val unit : cst
    (** The unit type. *)

    val base : cst
    (** An arbitrary type constant. *)

    val array : cst
    (** The type constant for arrays *)

    val bitv : int -> cst
    (** Bitvectors of the given length. *)

    val string : cst
    (** The type constant for strings *)

    val string_reg_lang : cst
    (** The type constant for regular languages over strings. *)

  end

end

(** {2 Terms} *)
(*  ************************************************************************* *)

module Term : sig

  (** {4 Type definition and usual functions} *)

  type t = term
  (** The type of terms and term variables. *)

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


  (** {4 tags} *)

  val tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given formula wiht the tag and value. *)

  val get_tag : t -> 'a tag -> 'a list
  (** Return the list of values associated to the tag. *)

  val get_tag_last : t -> 'a tag -> 'a option
  (** Return the last value associated to the tag (if any). *)


  (** {4 Term and types} *)

  val ty : t -> ty
  (** Returns the type of a term. *)

  val ensure : t -> ty -> t
  (** Ensure a term has the given type. *)


  (** {4 Free variables and substitutions} *)

  val fv : t -> Var.Set.t
  (** Returns the list of free variables in the formula. *)

  val subst : ?fix:bool -> t Var.Map.t -> t -> t
  (** Substitution over terms. *)


  (** {4 Generic Term creation functions} *)

  val of_var : var -> t
  (** Create a term from a variable *)

  val of_cst : cst -> t
  (** Create a term from a constant *)

  val apply : t -> t list -> t
  (** Higher-order application *)

  val apply_fo : cst -> ty list -> t list -> t
  (** Polymorphic first-order application. *)

  val pattern_match : t -> (pattern * t) list -> t
  (** Create a pattern match. *)

  val letin : (Var.t * t) list -> t -> t
  (** Let-binding. Variables can be bound to either terms or formulas. *)

  val all : var list -> t -> t
  (** Universal quantification. *)

  val ex : var list -> t -> t
  (** Existencial quantification *)



  (** {4 Builtins term creation} *)

  val void : t
  (** The only inhabitant of type unit. *)

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

  val ite : t -> t -> t -> t
  (** [ite condition then_t else_t] creates a conditional branch. *)

  (* Bitvector manipulation *)
  module Bitv : sig

    include Dolmen_intf.Term.Smtlib_Bitv with type t := t
    (** Satisfy the required interface for typing smtlib bitvectors. *)

    include Dolmen_intf.Term.Smtlib_Float_Bitv with type t := t
    (** Satisfy the required interface for typing smtlib floats. *)

  end

  (* Floating point number manipulations *)
  module Float : sig

    include Dolmen_intf.Term.Smtlib_Float_Float with type t := t
    (** Satisfy the required interface for typing smtlib floating points. *)

  end

  (** Integer operations. *)
  module Int : sig

    include Dolmen_intf.Term.Smtlib_Int with type t := t
    (** Satisfy the required interface for the typing of smtlib integers. *)

    include Dolmen_intf.Term.Tptp_Arith_Common with type t := t
    (** Satisfy the common interface for TPTP's arithmetic on integers. *)

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
    (** Satisfy the common interface for TPTP's arithmetic over Rationals *)

    val div : t -> t -> t
    (** Exact division on rationals. *)
  end

  (** Real operations *)
  module Real : sig

    include Dolmen_intf.Term.Smtlib_Real with type t := t
    (** Satisfy the required interface for the typing of smtlib's reals *)

    include Dolmen_intf.Term.Tptp_Arith_Common with type t := t
    (** Satisfy the common interface for TPTP's arithmetic over reals *)

    include Dolmen_intf.Term.Smtlib_Float_Real with type t := t
    (** Satisfy the real part of the SMTLIB's Float requirements *)

  end

  (** String operations *)
  module String : sig

    include Dolmen_intf.Term.Smtlib_String_String with type t := t
    (** Satisfy the required interface for the typing of smtlib's strings. *)

  end

end

