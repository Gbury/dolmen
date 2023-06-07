
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Temporary workaround for the fact that some of the module types included
   to simplify the documentation of the Term module ovarlap (i.e. multiple
   included signatures define the same function). *)
[@@@warning "-32"]

(** {2 Type definitions} *)
(*  ************************************************************************* *)


(** {3 Common definitions} *)

type hash = private int
type index = private int
type 'a tag = 'a Tag.t

(** {3 Type definitions} *)

type builtin = <
  ty : ty;
  ty_var : ty_var;
  ty_cst : ty_cst;
  term : term;
  term_var : term_var;
  term_cst : term_cst;
> Builtin.t
(** Extensible variant type for builtin operations. *)

and 'ty id = private {
  id_ty         : 'ty;
  index         : index; (** unique index *)
  path          : Path.t;
  builtin       : builtin;
  mutable tags  : Tag.map;
}
(** The type of identifiers. ['ty] is the type for representing the type of
    the id. *)

and type_ = Type

and type_fun = {
  arity : int;
  mutable alias : type_alias;
}

and type_alias =
  | No_alias
  | Alias of {
      alias_vars : ty_var list;
      alias_body : ty;
    }

and ty_var = type_ id
(** Abbreviation for type variables. *)

and ty_cst = type_fun id
(** Type symbols have the expected length of their argument encoded. *)

and ty_descr =
  | TyVar of ty_var             (** Type variables *)
  | TyApp of ty_cst * ty list   (** Application *)
  | Arrow of ty list * ty       (** Function type *)
  | Pi of ty_var list * ty      (** Type quantification *)
(** Type descriptions. *)

and ty = private {
  mutable ty_hash : hash; (* lazy hash *)
  mutable ty_tags : Tag.map;
  mutable ty_descr : ty_descr;
  mutable ty_head : ty;
}
(** Types, which wrap type description with a memoized hash and some tags. *)

and term_var = ty id
(** Term variables *)

and term_cst = ty id
(** Term symbols, which encode their expected type and term argument lists lengths. *)

and pattern = term
(** patterns are simply terms *)

and term_descr =
  | Var of term_var                         (** Variables *)
  | Cst of term_cst                         (** Constants *)
  | App of term * ty list * term list       (** Application *)
  | Binder of binder * term                 (** Binders *)
  | Match of term * (pattern * term) list   (** Pattern matching *)
(** Term descriptions. *)

and binder =
  | Let_seq of (term_var * term) list
  | Let_par of (term_var * term) list
  | Lambda  of ty_var list * term_var list
  | Exists  of ty_var list * term_var list
  | Forall  of ty_var list * term_var list (**)
(** Binders. *)

and term = {
  term_ty : ty;
  term_descr : term_descr;
  mutable term_hash : hash;
  mutable term_tags : Tag.map;
}
(** Term, which wrap term descriptions. *)

and formula = term
(** Alias for signature compatibility (with Dolmen_loop.Pipes.Make for instance). *)

type ty_def_adt_case = {
  cstr : term_cst;
  tester : term_cst;
  dstrs : term_cst option array;
}
(** One case of the type definition for an algebraic datatype. *)

type ty_def =
  | Abstract
  (** Abstract types *)
  | Adt of {
      ty : ty_cst;
      record : bool;
      cases : ty_def_adt_case array;
    }
  (** Algebraic datatypes, including records (which are seen as a 1-case adt). *)
(** Type definitions. *)


(** {2 Exceptions} *)
(*  ************************************************************************* *)

exception Already_aliased of ty_cst
exception Type_already_defined of ty_cst
exception Record_type_expected of ty_cst
exception Wildcard_already_set of ty_var


(** {2 Native Tags} *)
(*  ************************************************************************* *)

module Tags : sig

  type 'a t = 'a tag
  (** Polymorphic tags *)

  val bound : term tag
  (** Tag used one let-bound variables to reference the defining term for
      the variable (i.e. the term to which it is let-bound). *)

  include Dolmen_intf.Tag.Smtlib_Base with type 'a t := 'a t
                                       and type term := term
  (** Satsify the Smtlib interface. *)

  include Dolmen_intf.Tag.Zf_Base with type 'a t := 'a t
                                   and type pos = Pretty.pos
  (** Satsify the Zf interface. *)

  include Dolmen_intf.Tag.Ae_Base with type 'a t := 'a t
                                        and type term := term
  (** Satsify the Ae interface. *)

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

  val type_ : type_ t
  (** Printer for type_. *)

  val type_fun : type_fun t
  (** Printer for type_fun. *)

  val ty_var : ty_var t
  (** Printer to print type variables along with their types. *)

  val ty_cst : ty_cst t
  (** Printer to print type constants along with their types. *)

  val term_var : term_var t
  (** Printer to print term variables along with their types. *)

  val term_cst : term_cst t
  (** Printer to print term constants along with their types. *)

  val ty : ty t
  (** Printer for types. *)

  val term : term t
  (** Printer for terms. *)

  val formula : formula t
  (** Printer for formulas. *)

  val ty_def : ty_def t
  (** Printer for type definitions. *)

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

(** {2 Identifiers} *)
(*  ************************************************************************* *)

module Id : sig

  type 'a t = 'a id
  (** The type of identifiers *)

  val hash : 'a t -> int
  (** Hash function. *)

  val equal : 'a t -> 'b t -> bool
  (** Equality function. *)

  val compare : 'a t -> 'b t -> int
  (** Comparison function. *)

  val print : Format.formatter -> 'a t -> unit
  (** Printing function *)

  val mk :
    ?pos:Pretty.pos ->
    ?name:string ->
    ?tags:Tag.map ->
    ?builtin:builtin ->
    Path.t -> 'a -> 'a t
  (** Create a new identifier *)

  val get_tag : _ t -> 'a Tag.t -> 'a option
  (** Get the value bound to a tag. *)

  val get_tag_list : _ t -> 'a list Tag.t -> 'a list
  (** Get the list of values bound to a list tag, returning the
      empty list if no value is bound. *)

  val get_tag_last : _ t -> 'a list Tag.t -> 'a option
  (** Get the last value bound to a list tag. *)

  val set_tag : _ t -> 'a Tag.t -> 'a -> unit
  (** Set the value bound to the tag. *)

  val add_tag : _ t -> 'a list Tag.t -> 'a -> unit
  (** Bind an additional value to a list tag. *)

  val add_tag_opt : _ t -> 'a list Tag.t -> 'a option -> unit
  (** Optionally bind an additional value to a list tag. *)

  val add_tag_list : _ t -> 'a list Tag.t -> 'a list -> unit
  (** Bind a list of additional values to a list tag. *)

  val unset_tag : _ t -> _ Tag.t -> unit
  (** Remove the binding to the given tag. *)

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

  exception Bad_arity of ty_cst * t list
  (** Raised when applying a type constant to the wrong number
      of arguments. *)

  exception Prenex_polymorphism of t
  (** Raised when the type provided is polymorphic, but occurred in a
      place where polymorphic types are forbidden by prenex/rank-1
      polymorphism. *)

  val hash : t -> int
  (** A hash function for types, should be suitable to create hashtables. *)

  val equal : t -> t -> bool
  (** An equality function on types. Should be compatible with the hash function. *)

  val compare : t -> t -> int
  (** Comparison function over types. Should be compativle with the equality function. *)

  val print : Format.formatter -> t -> unit
  (** Printing function. *)


  (** {4 Alias management} *)

  val alias_to : ty_cst -> ty_var list -> ty -> unit
  (** Alias the given type constant. *)

  val expand_head : t -> t
  (** Expand head aliases. *)


  (** {4 View} *)

  type view = [
    | `Prop
    (** Propositions/booleans *)
    | `Int
    (** Integers *)
    | `Rat
    (** Rationals *)
    | `Real
    (** Reals *)
    | `Array of ty * ty
    (** Function arrays, from source to destination type. *)
    | `Bitv of int
    (** Bitvectors of fixed length. *)
    | `Float of int * int
    (** Floating points. *)
    | `String
    (** Strings *)
    | `String_reg_lang
    (** Regular languages over strings *)
    | `Var of ty_var
    (** Variables (excluding wildcards) *)
    | `Wildcard of ty_var
    (** Wildcards *)
    | `App of [
        | `Generic of ty_cst
        | `Builtin of builtin
      ] * ty list
    (** Generic applications. *)
    | `Arrow of ty list * ty
    | `Pi of ty_var list * ty
  ]
  (** View on types. *)

  val view : t -> view
  (** View on types. *)

  val pi_arity : t -> int
  (** Returns the number of expected type arguments that the given
      type expects (i.e. the number of prenex polymorphic variables
      in the given type). *)

  val poly_sig : t -> ty_var list * ty list * ty
  (** Split a type into a polymorphic signature. *)


  (** {4 Type structure definition} *)

  type def = ty_def
  (** Alias for type definitions. *)

  val define : ty_cst -> def -> unit
  (** Register a type definition. *)

  val definition : ty_cst -> def option
  (** Return the definition of a type (if it exists). *)


  (** {4 Variables and constants} *)

  (** A module for variables that occur in types. *)
  module Var : sig

    type t = ty_var
    (** The type of variables the can occur in types *)

    val print : Format.formatter -> t -> unit
    (** Printer. *)

    val hash : t -> int
    (** A hash function for type variables, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on type variables. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val mk : string -> t
    (** Create a new type variable with the given name. *)

    val wildcard : unit -> t
    (** Type wildcard *)

    val is_wildcard : t -> bool
    (** Predictae to distinguish wildcard type variables. *)

    val get_tag : t -> 'a Tag.t -> 'a option
    (** Get the value bound to a tag. *)

    val get_tag_list : t -> 'a list Tag.t -> 'a list
    (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

    val get_tag_last : t -> 'a list Tag.t -> 'a option
    (** Get the last value bound to a list tag. *)

    val set_tag : t -> 'a Tag.t -> 'a -> unit
    (** Set the value bound to the tag. *)

    val add_tag : t -> 'a list Tag.t -> 'a -> unit
    (** Bind an additional value to a list tag. *)

    val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
    (** Optionally bind an additional value to a list tag. *)

    val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
    (** Bind a list of additional values to a list tag. *)

    val unset_tag : t -> _ Tag.t -> unit
    (** Remove the binding to the given tag. *)

  end

  (** A module for constant symbols the occur in types. *)
  module Const : sig

    type t = ty_cst
    (** The type of constant symbols the can occur in types *)

    val print : Format.formatter -> t -> unit
    (** Printer. *)

    val hash : t -> int
    (** A hash function for type constants, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on type constants. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val arity : t -> int
    (** Return the arity of the given symbol. *)

    val mk : Path.t -> int -> t
    (** Create a type constant with the given arity. *)

    val get_tag : t -> 'a Tag.t -> 'a option
    (** Get the value bound to a tag. *)

    val get_tag_list : t -> 'a list Tag.t -> 'a list
    (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

    val get_tag_last : t -> 'a list Tag.t -> 'a option
    (** Get the last value bound to a list tag. *)

    val set_tag : t -> 'a Tag.t -> 'a -> unit
    (** Set the value bound to the tag. *)

    val add_tag : t -> 'a list Tag.t -> 'a -> unit
    (** Bind an additional value to a list tag. *)

    val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
    (** Optionally bind an additional value to a list tag. *)

    val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
    (** Bind a list of additional values to a list tag. *)

    val unset_tag : t -> _ Tag.t -> unit
    (** Remove the binding to the given tag. *)

    val int : t
    (** The type constant for integers *)

    val rat : t
    (** The type constant for rationals *)

    val real : t
    (** The type constant for reals. *)

    val prop : t
    (** The type constant for propositions *)

    val unit : t
    (** The unit type. *)

    val base : t
    (** An arbitrary type constant. *)

    val array : t
    (** The type constant for arrays *)

    val bitv : int -> t
    (** Bitvectors of the given length. *)

    val string : t
    (** The type constant for strings *)

    val string_reg_lang : t
    (** The type constant for regular languages over strings. *)

  end

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

  val of_var : Var.t -> t
  (** Create a type from a variable. *)

  val apply : Const.t -> t list -> t
  (** Application for types. *)

  val arrow : t list -> t -> t
  (** Create an arrow type (i.e. function type) *)

  val pi : Var.t list -> t -> t
  (** Create a prenex/rank-1 polymorphic type. *)

  val array : t -> t -> t
  (** Build an array type from source to destination types. *)

  val bitv : int -> t
  (** Bitvectors of a given length. *)

  val float : int -> int -> t
  (** Floating point of given exponent and significand. *)

  val roundingMode : t
  (** Type for the various Floating point rounding modes. *)

  val subst : ?fix:bool -> subst -> t -> t
  (** Substitution on types. *)

  val fv : t -> Var.t list
  (** Returns the list of free variables in the type. *)

  val freshen : t -> t
  (** Freshen a type, replacing all its bound variables by new/fresh
      type variables. *)

  val unify : t -> t -> t option
  (** Try and unify two types. *)

  val match_ : t list -> t list -> subst option
  (** Try and pattern mathc a list of patterns agains a list of types. *)

  val instance_of : t -> t -> t list option
  (** [instance_of poly t] decides whether [t] is an instance of [poly],
      that is whether there are some types [l] such that a term of
      type [poly] applied to type arguments [l] gives a term of type
      [t]. *)

  val set_wildcard : ty_var -> t -> unit
  (** Instantiate the given wildcard. *)

  val add_wildcard_hook : hook:(ty_var -> ty -> unit) -> ty_var -> unit
  (** Tag for hooks called upon the wildcard instantiation. *)

  val get_tag : t -> 'a Tag.t -> 'a option
  (** Get the value bound to a tag. *)

  val get_tag_list : t -> 'a list Tag.t -> 'a list
  (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

  val get_tag_last : t -> 'a list Tag.t -> 'a option
  (** Get the last value bound to a list tag. *)

  val set_tag : t -> 'a Tag.t -> 'a -> unit
  (** Set the value bound to the tag. *)

  val add_tag : t -> 'a list Tag.t -> 'a -> unit
  (** Bind an additional value to a list tag. *)

  val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
  (** Optionally bind an additional value to a list tag. *)

  val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
  (** Bind a list of additional values to a list tag. *)

  val unset_tag : t -> _ Tag.t -> unit
  (** Remove the binding to the given tag. *)

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

  val get_tag : t -> 'a Tag.t -> 'a option
  (** Get the value bound to a tag. *)

  val get_tag_list : t -> 'a list Tag.t -> 'a list
  (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

  val get_tag_last : t -> 'a list Tag.t -> 'a option
  (** Get the last value bound to a list tag. *)

  val set_tag : t -> 'a Tag.t -> 'a -> unit
  (** Set the value bound to the tag. *)

  val add_tag : t -> 'a list Tag.t -> 'a -> unit
  (** Bind an additional value to a list tag. *)

  val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
  (** Optionally bind an additional value to a list tag. *)

  val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
  (** Bind a list of additional values to a list tag. *)

  val unset_tag : t -> _ Tag.t -> unit
  (** Remove the binding to the given tag. *)

  (** A module for variables that occur in terms. *)
  module Var : sig

    type t = term_var
    (** The type of variables the can occur in terms *)

    val print : Format.formatter -> t -> unit
    (** Printer. *)

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

    val get_tag : t -> 'a Tag.t -> 'a option
    (** Get the value bound to a tag. *)

    val get_tag_list : t -> 'a list Tag.t -> 'a list
    (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

    val get_tag_last : t -> 'a list Tag.t -> 'a option
    (** Get the last value bound to a list tag. *)

    val set_tag : t -> 'a Tag.t -> 'a -> unit
    (** Set the value bound to the tag. *)

    val add_tag : t -> 'a list Tag.t -> 'a -> unit
    (** Bind an additional value to a list tag. *)

    val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
    (** Optionally bind an additional value to a list tag. *)

    val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
    (** Bind a list of additional values to a list tag. *)

    val unset_tag : t -> _ Tag.t -> unit
    (** Remove the binding to the given tag. *)

  end

  (** A module for constant symbols that occur in terms. *)
  module Const : sig

    type t = term_cst
    (** The type of constant symbols that can occur in terms *)

    val print : Format.formatter -> t -> unit
    (** Printer. *)

    val hash : t -> int
    (** A hash function for term constants, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on term constants. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val ty : t -> ty
    (** Returns the type of a term constant. *)

    val mk : Path.t -> ty -> t
    (** Create a constant symbol. *)

    val get_tag : t -> 'a Tag.t -> 'a option
    (** Get the value bound to a tag. *)

    val get_tag_list : t -> 'a list Tag.t -> 'a list
    (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

    val get_tag_last : t -> 'a list Tag.t -> 'a option
    (** Get the last value bound to a list tag. *)

    val set_tag : t -> 'a Tag.t -> 'a -> unit
    (** Set the value bound to the tag. *)

    val add_tag : t -> 'a list Tag.t -> 'a -> unit
    (** Bind an additional value to a list tag. *)

    val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
    (** Optionally bind an additional value to a list tag. *)

    val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
    (** Bind a list of additional values to a list tag. *)

    val unset_tag : t -> _ Tag.t -> unit
    (** Remove the binding to the given tag. *)

    include Dolmen_intf.Term.Tptp_Thf_Core_Const with type t := t
    (** Satisfy the required interface for the typing of tptp's Thf. *)

    val eqs : int -> t
    (** n-ary equality. *)

    val distinct : int -> t
    (** n-ary disequality. *)

    val _and : int -> t
    (** n-ary conjonction. *)

    val _or : int -> t
    (** n-ary disjunction. *)

    val coerce : t
    (** Type coercion. *)

    val in_interval : bool * bool -> t
    (** Interger interval inclusion. *)

    val maps_to : t
    (** Mapping (used in triggers).  *)

    (** A module for integer constant symbols that occur in terms. *)
    module Int : sig

      val int : string -> t
      (** Integer literals. *)

      val minus : t
      (** Integer unary minus/negation. *)

      val add : t
      (** Integer addition. *)

      val sub : t
      (** Integer subtraction. *)

      val mul : t
      (** Integer multiplication. *)

      val pow : t
      (** Integer exponentiation. *)

      val div_e : t
      (** Integer euclidian division quotient. *)

      val div_t : t
      (** Truncation of the integer division. *)

      val div_f : t
      (** Floor of the integer divison. *)

      val rem_e : t
      (** Integer euclidian division remainder. *)

      val rem_t : t
      (** Remainder of the integer division. *)

      val rem_f : t
      (** Floor of the integer division. *)

      val abs : t
      (** Integer absolute value. *)

      val lt : t
      (** Integer "less than" comparison. *)

      val le : t
      (** Integer "less or equal" comparison. *)

      val gt : t
      (** Integer "greater than" comparison. *)

      val ge : t
      (** Integer "greater or equal" comparison. *)

      val floor : t
      (** Integer floor function. *)

      val ceiling : t
      (** Integer ceiling function. *)

      val truncate : t
      (** Integer truncation function. *)

      val round : t
      (** Integer rounding function. *)

      val is_int : t
      (** Integer testing. *)

      val is_rat : t
      (** Rationality testing. *)

      val divisible : t
      (** Arithmetic divisibility testing. *)
    end

    (** A module for rational constant symbols that occur in terms. *)
    module Rat : sig

      val rat : string -> t
      (** Rational literals *)

      val minus : t
      (** Rational unary minus/negation. *)

      val add : t
      (** Rational addition. *)

      val sub : t
      (** Rational subtraction. *)

      val mul : t
      (** Rational multiplication. *)

      val div_e : t
      (** Rational euclidian division quotient. *)

      val div_t : t
      (** Truncation of the rational division. *)

      val div_f : t
      (** Floor of the rational divison. *)

      val rem_e : t
      (** Euclidian division remainder. *)

      val rem_t : t
      (** Remainder of the rational division. *)

      val rem_f : t
      (** Floor of the rational division. *)

      val lt : t
      (** Rational "less than" comparison. *)

      val le : t
      (** Rational "less or equal" comparison. *)

      val gt : t
      (** Rational "greater than" comparison. *)

      val ge : t
      (** Rational "greater or equal" comparison. *)

      val floor : t
      (** Rational floor function. *)

      val ceiling : t
      (** Rational ceiling function. *)

      val truncate : t
      (** Rational truncation function. *)

      val round : t
      (** Rational rounding function. *)

      val is_int : t
      (** Integer testing. *)

      val is_rat : t
      (** Rationality testing. *)
    end

    (** A module for real constant symbols that occur in terms. *)
    module Real : sig
      val real : string -> t
      (** Real literals. *)

      val minus : t
      (** Real unary minus/negation. *)

      val add : t
      (** Real addition. *)

      val sub : t
      (** Real subtraction. *)

      val mul : t
      (** Real multiplication. *)

      val pow : t
      (** Real exponentiation. *)

      val div : t
      (** Real division. *)

      val div_e : t
      (** Real euclidian division quotient. *)

      val div_t : t
      (** Truncation of the real division. *)

      val div_f : t
      (** Floor of the real divison. *)

      val rem_e : t
      (** Real euclidian division remainder. *)

      val rem_t : t
      (** Remainder of the real division. *)

      val rem_f : t
      (** Floor of the real division. *)

      val lt : t
      (** Real "less than" comparison. *)

      val le : t
      (** Real "less or equal" comparison. *)

      val gt : t
      (** Real "greater than" comparison. *)

      val ge : t
      (** Real "greater or equal" comparison. *)

      val floor : t
      (** Real floor function. *)

      val floor_to_int : t
      (** Real floor to integer function. *)

      val ceiling : t
      (** Real ceiling function. *)

      val truncate : t
      (** Real truncation function. *)

      val round : t
      (** Real rounding function. *)

      val is_int : t
      (** Integer testing. *)

      val is_rat : t
      (** Rationality testing. *)
    end

    (** A module for array constant symbols that occur in terms. *)
    module Array: sig

      val const : t
      (** Array selection. *)

      val select : t
      (** Array selection. *)

      val store : t
      (** Array store. *)

    end

    (** A module for bit vector constant symbols that occur in terms. *)
    module Bitv : sig
      val bitv : string -> t
      (** Bitvetor literals. *)

      val concat : int * int -> t
      (** Bitvector concatenation. *)

      val extract : int * int * int -> t
      (** Bitvector extraction. *)

      val repeat : int * int -> t
      (** Bitvector repetition. *)

      val zero_extend : int * int -> t
      (** Bitvector extension with zeros. *)

      val sign_extend : int * int -> t
      (** Bitvector extension with its most significant. *)

      val rotate_right : int * int -> t
      (** Bitvector rotation to the right. *)

      val rotate_left : int * int -> t
      (** Bitvector rotation to the left. *)

      val not : int -> t
      (** Bitwise negation. *)

      val and_ : int -> t
      (** Bitwise conjunction. *)

      val or_ : int -> t
      (** Bitwise disjunction. *)

      val nand : int -> t
      (** Bitwise nand. *)

      val nor : int -> t
      (** Bitwise nor. *)

      val xor : int -> t
      (** Bitwise xor. *)

      val xnor : int -> t
      (** Bitwise xnor. *)

      val comp : int -> t
      (** Bitwise comparison. *)

      val neg : int -> t
      (** Arithmetic complement on bitvectors. *)

      val add : int -> t
      (** Arithmetic addition on bitvectors. *)

      val sub : int -> t
      (** Arithmetic substraction on bitvectors. *)

      val mul : int -> t
      (** Arithmetic multiplication on bitvectors. *)

      val udiv : int -> t
      (** Arithmetic euclidian integer division on bitvectors. *)

      val urem : int -> t
      (** Arithmetic euclidian integer remainder on bitvectors. *)

      val sdiv : int -> t
      (** Arithmetic 2's complement signed division.
          (see smtlib's specification for more information).*)

      val srem : int -> t
      (** Arithmetic 2's coplement signed remainder (sign follows dividend).
          (see smtlib's specification for more information).*)

      val smod : int -> t
      (** Arithmetic 2's coplement signed remainder (sign follows divisor).
          (see smtlib's specification for more information). *)

      val shl : int -> t
      (** Logical shift left. *)

      val lshr : int -> t
      (** Logical shift right. *)

      val ashr : int -> t
      (** Arithmetic shift right. *)

      val ult : int -> t
      (** Boolean arithmetic comparison (less than). *)

      val ule : int -> t
      (** Boolean arithmetic comparison (less or equal than). *)

      val ugt : int -> t
      (** Boolean arithmetic comparison (greater than). *)

      val uge : int -> t
      (** Boolean arithmetic comparison (greater or equal than). *)

      val slt : int -> t
      (** Boolean signed arithmetic comparison (less than).
          (See smtlib's specification for more information). *)

      val sle : int -> t
      (** Boolean signed arithmetic comparison (less or equal than). *)

      val sgt : int -> t
      (** Boolean signed arithmetic comparison (greater than). *)

      val sge : int -> t
      (** Boolean signed arithmetic comparison (greater or equal than). *)

    end

    (** A module for floating point constant symbols that occur in terms. *)
    module Float : sig
      val fp : int * int -> t
      (** Floating point literal. *)

      val roundNearestTiesToEven: t
      (** Constant for rounding mode RNE. *)

      val roundNearestTiesToAway: t
      (** Constant for rounding mode RNA. *)

      val roundTowardPositive: t
      (** Constant for rounding mode RTP. *)

      val roundTowardNegative: t
      (** Constant for rounding mode RTN. *)

      val roundTowardZero: t
      (** Constant for rounding mode RTZ. *)

      val plus_infinity : int * int -> t
      (** The constant plus infinity, it is also equivalent to a literal. *)

      val minus_infinity : int * int -> t
      (** The constant minus infinity, it is also equivalent to a literal. *)

      val plus_zero : int * int -> t
      (** The constant plus zero, it is also equivalent to a literal. *)

      val minus_zero : int * int -> t
      (** The constant minus zero, it is also equivalent to a literal. *)

      val nan : int * int -> t
      (** The constant Non-numbers, it is also equivalent to many literals which are
          equivalent together. *)

      val abs : int * int -> t
      (** Absolute value. *)

      val neg : int * int -> t
      (** Floating point negation. *)

      val add : int * int -> t
      (** Floating point addition. *)

      val sub : int * int -> t
      (** Floating point subtraction. *)

      val mul : int * int -> t
      (** Floating point multiplication. *)

      val div : int * int -> t
      (** Floating point division. *)

      val fma : int * int -> t
      (** Floating point fused multiplication and addition. *)

      val sqrt : int * int -> t
      (** Floating point square root. *)

      val rem : int * int -> t
      (** Floating point division remainder. *)

      val roundToIntegral : int * int -> t
      (** Floating point rounding to integral. *)

      val min : int * int -> t
      (** Floating point minimum. *)

      val max : int * int -> t
      (** Floating point maximum. *)

      val lt : int * int -> t
      (** Floating point "less than" comparison. *)

      val leq : int * int -> t
      (** Floating point "less or equal" comparison. *)

      val gt : int * int -> t
      (** Floating point "greater than" comparison. *)

      val geq : int * int -> t
      (** Floating point "greater or equal" comparison. *)

      val eq : int * int -> t
      (** Floating point equality. *)

      val isNormal : int * int -> t
      (** Test if a value is a normal floating point. *)

      val isSubnormal : int * int -> t
      (** Test if a value is a subnormal floating point. *)

      val isZero : int * int -> t
      (** Test if a value is a zero. *)

      val isInfinite : int * int -> t
      (** Test if a value is an infinite. *)

      val isNaN : int * int -> t
      (** Test if a value is NaN. *)

      val isNegative : int * int -> t
      (** Test if a value is a negative floating point. *)

      val isPositive : int * int -> t
      (** Test if a value is a positive floating point. *)

      val to_real : int * int -> t
      (** Convert a floating point to a real. *)

      val ieee_format_to_fp : int * int -> t
      (** Convert a bitvector into a floating point using IEEE 754-2008 interchange format. *)

      val to_fp : int * int * int * int -> t
      (** Convert from one floating point format to another. *)

      val real_to_fp : int * int -> t
      (** Convert a real to a floating point. *)

      val sbv_to_fp : int * int * int -> t
      (** Convert a signed bitvector to a floating point. *)

      val ubv_to_fp : int * int * int -> t
      (** Convert an unsigned bitvector to a floating point. *)

      val to_ubv : int * int * int -> t
      (** Convert an floating point to an unsigned bitvector. *)

      val to_sbv : int * int * int -> t
      (** Convert an floating point to an signed bitvector. *)

    end

    (** A module for string constant symbols that occur in terms. *)
    module String : sig
      val string : string -> t
      (** String literal. *)

      val length : t
      (** String length. *)

      val at : t
      (** Get a char in a string. *)

      val to_code : t
      (** Returns the code point of a the single character of the string
          or [(-1)] if the string is not a singleton. *)

      val of_code : t
      (** Returns the singleton string whose only character is the given
          code point. *)

      val is_digit : t
      (** Check if the string a singleton string with a single digit character. *)

      val to_int : t
      (** Evaluates the string as a decimal natural number, or [(-1)] if
          it's not possible. *)

      val of_int : t
      (** Convert an int expression to a string in decimal representation. *)

      val concat : t
      (** String concatenation. *)

      val sub : t
      (** Substring extraction. *)

      val index_of : t
      (** Index of the first occurrence of the second string in
          first one, starting at the position of the third argument. *)

      val replace : t
      (** Replace the first occurrence. *)

      val replace_all : t
      (** Replace all occurrences. *)

      val replace_re : t
      (** Replace the leftmost, shortest re ocurrence. *)

      val replace_re_all : t
      (** Replace left-to-right, each shortest non empty re occurrence. *)

      val is_prefix : t
      (** Check if the first string is a prefix of the second one. *)

      val is_suffix : t
      (** Check if the first string is a suffix of the second one. *)

      val contains : t
      (** Check if the first string contains the second one. *)

      val lt : t
      (** Check for lexicographic strict ordering. *)

      val leq : t
      (** Check for lexicographic large ordering. *)

      val in_re : t
      (** Check if the string is in regular language. *)

      (** A module for regular language constant symbols that occur in terms. *)
      module Reg_Lang : sig
        val empty : t
        (** The empty regular language. *)

        val all : t
        (** The language that contains all strings. *)

        val allchar : t
        (** The language that contains all strings of length 1. *)

        val of_string : t
        (** Singleton language containing a single string. *)

        val range : t
        (** [range s1 s2] is the language containing all singleton strings
            (i.e. string of length 1) that are lexicographically beetween
            [s1] and [s2], **assuming [s1] and [s2] are singleton strings**.
            Else it is the empty language. *)

        val concat : t
        (** Language concatenation. *)

        val union : t
        (** Language union. *)

        val inter : t
        (** Language intersection. *)

        val diff : t
        (** Language difference. *)

        val star : t
        (** Kleene closure. *)

        val cross : t
        (** Kleene cross. [cross e] abbreviates [concat e (star e)]. *)

        val complement : t
        (** Language complement. *)

        val option : t
        (** Option. [option e] abbreviates [union e (of_string "")]. *)

        val power : int -> t
        (** [power n e] is [n]-th power of [e]. *)

        val loop : int * int -> t
        (** Loop. See SMTLIb documentation. *)

      end
    end
  end

  (** A module for Algebraic datatype constructors. *)
  module Cstr : sig

    type t = term_cst
    (** An algebraic type constructor. Note that such constructors are used to
        build terms, and not types, e.g. consider the following:
        [type 'a list = Nil | Cons of 'a * 'a t], then [Nil] and [Cons] are the
        constructors, while [list] would be a type constant of arity 1 used to
        name the type. *)

    val print : Format.formatter -> t -> unit
    (** Printer. *)

    val hash : t -> int
    (** A hash function for adt constructors, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on adt constructors. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val ty : t -> ty
    (** Returns the type of a constructor. *)

    val void : t
    (** Only constructor for the type unit. *)

    val pattern_arity : t -> ty -> ty list -> ty list
    (** Used in the type-checking of pattern matching.
        [pattern_arity cstr ret ty_args] should return the types of the expected arguments
        [args] such that [apply_cstr cstr ty_args args] has type [ret].
        @raise Wrong_sum_type if [ret] cannot be unified with the type of [c]
        @raise Bad_term_arity if the provided type argument list is not of the correct length
    *)

    val get_tag : t -> 'a Tag.t -> 'a option
    (** Get the value bound to a tag. *)

    val get_tag_list : t -> 'a list Tag.t -> 'a list
    (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

    val get_tag_last : t -> 'a list Tag.t -> 'a option
    (** Get the last value bound to a list tag. *)

    val set_tag : t -> 'a Tag.t -> 'a -> unit
    (** Set the value bound to the tag. *)

    val add_tag : t -> 'a list Tag.t -> 'a -> unit
    (** Bind an additional value to a list tag. *)

    val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
    (** Optionally bind an additional value to a list tag. *)

    val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
    (** Bind a list of additional values to a list tag. *)

    val unset_tag : t -> _ Tag.t -> unit
    (** Remove the binding to the given tag. *)

  end

  (** A module for Record fields. *)
  module Field : sig

    type t = term_cst
    (** A record field. *)

    val print : Format.formatter -> t -> unit
    (** Printer. *)

    val hash : t -> int
    (** A hash function for adt destructors. *)

    val equal : t -> t -> bool
    (** An equality function on adt destructors. *)

    val compare : t -> t -> int
    (** A comparison function on adt constructors. *)

    val get_tag : t -> 'a Tag.t -> 'a option
    (** Get the value bound to a tag. *)

    val get_tag_list : t -> 'a list Tag.t -> 'a list
    (** Get the list of values bound to a list tag, returning the
        empty list if no value is bound. *)

    val get_tag_last : t -> 'a list Tag.t -> 'a option
    (** Get the last value bound to a list tag. *)

    val set_tag : t -> 'a Tag.t -> 'a -> unit
    (** Set the value bound to the tag. *)

    val add_tag : t -> 'a list Tag.t -> 'a -> unit
    (** Bind an additional value to a list tag. *)

    val add_tag_opt : t -> 'a list Tag.t -> 'a option -> unit
    (** Optionally bind an additional value to a list tag. *)

    val add_tag_list : t -> 'a list Tag.t -> 'a list -> unit
    (** Bind a list of additional values to a list tag. *)

    val unset_tag : t -> _ Tag.t -> unit
    (** Remove the binding to the given tag. *)

  end

  val define_record :
    ty_const -> ty_var list -> (Path.t * ty) list -> Ty.def * Field.t list
  (** Define a new record type. *)

  val define_adt :
    ty_const -> ty_var list ->
    (Path.t * (ty * Path.t option) list) list ->
    Ty.def * (Cstr.t * (ty * Const.t option) list) list
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

  exception Field_expected of term_cst
  (** A field was expected but the returned term constant is not a record field. *)

  exception Pattern_expected of t
  (** Raised when trying to create a pattern matching, but a non-pattern term
      was provided where a pattern was expected. *)

  exception Empty_pattern_matching
  (** Raise when creating a pattern matching but an empty list of branches
      was provided *)

  exception Partial_pattern_match of t list
  (** Raised when a partial pattern matching was created. A list of terms not
      covered by the patterns is provided. *)

  exception Constructor_expected of Cstr.t
  (** Raised when trying to access the tester of an ADT constructor, but the constant
      provided was not a constructor. *)

  exception Over_application of t list
  (** Raised when an application was provided too many term arguments. The
      extraneous arguments are returned by the exception. *)

  exception Bad_poly_arity of ty_var list * ty list
  (** Raised when a polymorphic application does not have an
      adequate number of arguments. *)


  val ensure : t -> ty -> t
  (** Ensure a term has the given type. *)

  val of_var : Var.t -> t
  (** Create a term from a variable *)

  val of_cst : Const.t -> t
  (** Create a term from a constant. *)

  val apply : t -> ty list -> t list -> t
  (** Polymorphic application. *)

  val apply_cst : term_cst -> ty list -> t list -> t
  (** Polymorphic application of a constructor. *)

  val apply_cstr : Cstr.t -> ty list -> t list -> t
  (** Polymorphic application of a constructor. *)

  val apply_field : Field.t -> t -> t
  (** Field access for a record. *)

  val cstr_tester : Cstr.t -> t -> t
  (** Test expression for a constructor. *)

  val pattern_match :
    ?redundant:(pattern -> unit) -> t -> (pattern * t) list -> t
  (** Create a pattern match. *)

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

  val neq : t -> t -> t
  (** Disequality *)

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

  val implied : t -> t -> t
  (** Reverse Implication *)

  val equiv : t -> t -> t
  (** Equivalence *)

  val lam : ty_var list * Var.t list -> t -> t
  (** Create a local function.
      The first pair of arguments are the variables that are free in the resulting
      quantified formula, and the second pair are the variables bound. *)

  val all : ty_var list * Var.t list -> t -> t
  (** Universally quantify the given formula over the type and terms variables.
      The first pair of arguments are the variables that are free in the resulting
      quantified formula, and the second pair are the variables bound. *)

  val ex : ty_var list * Var.t list -> t -> t
  (** Existencially quantify the given formula over the type and terms variables.
      The first pair of arguments are the variables that are free in the resulting
      quantified formula, and the second pair are the variables bound. *)

  val bind : Var.t -> t -> t
  (** Tag the given variable with the term, to mark it has been let-bound.
      Views might use that information to transparently replace a let-bound
      variable with its defining term. *)

  val letin : (Var.t * t) list -> t -> t
  (** Sequential let-binding. Variables can be bound to either terms or formulas. *)

  val letand : (Var.t * t) list -> t -> t
  (** Parrallel let-binding. Variables can be bound to either terms or formulas. *)

  val ite : t -> t -> t -> t
  (** [ite condition then_t else_t] creates a conditional branch. *)

  val fv : t -> ty_var list * Var.t list
  (** Returns the list of free variables in the formula. *)

  val subst : ?fix:bool -> Ty.subst -> subst -> t -> t
  (** Substitution over terms. *)

  (* Alt-Ergo's semantic triggers *)

  val in_interval : t -> bool * bool -> t -> t -> t
  (** Interger interval inclusion. *)

  val maps_to : Var.t -> t -> t
  (** Variable mapping to term. *)

  (* Array manipulation *)
  module Array : sig
    val const : ty -> t -> t
    (** [const index_ty base] creates a constant array that maps any value of type
        [index_ty] to the value [base]. *)

    val select : t -> t -> t
    (** Array selection. *)

    val store : t -> t -> t -> t
    (** Array store. *)
  end

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
                                                 and type cst := term_cst
    (** Satisfy the required interface for typing smtlib floating points. *)

  end

  (** Integer operations. *)
  module Int : sig

    include Dolmen_intf.Term.Smtlib_Int with type t := t
                                         and type cst := term_cst
    (** Satisfy the required interface for the typing of smtlib integers. *)

    include Dolmen_intf.Term.Tptp_Tff_Arith_Common with type t := t
    (** Satisfy the common interface for TPTP's arithmetic on integers. *)

    include Dolmen_intf.Term.Ae_Arith_Common with type t := t
    (** Satisfy the common interface for Alt-Ergo's arithmetic types. *)

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

    include Dolmen_intf.Term.Tptp_Tff_Arith_Common with type t := t
    (** Satisfy the common interface for TPTP's arithmetic over Rationals *)

    val mk : string -> t
    (** Create a rational constant. *)

    val div : t -> t -> t
    (** Exact division on rationals. *)

  end

  (** Real operations *)
  module Real : sig

    include Dolmen_intf.Term.Smtlib_Real with type t := t
                                          and type cst := term_cst
    (** Satisfy the required interface for the typing of smtlib's reals *)

    include Dolmen_intf.Term.Tptp_Tff_Arith_Common with type t := t
    (** Satisfy the common interface for TPTP's arithmetic over reals *)

    include Dolmen_intf.Term.Smtlib_Float_Real with type t := t
    (** Satisfy the real part of the SMTLIB's Float requirements *)

    include Dolmen_intf.Term.Ae_Arith_Common with type t := t
    (** Satisfy the common interface for Alt-Ergo's arithmetic types. *)

    val floor_to_int : t -> t
    (** Greatest integer smaller than the given real *)

  end

  (** String operations *)
  module String : sig

    include Dolmen_intf.Term.Smtlib_String_String with type t := t
    (** Satisfy the required interface for the typing of smtlib's strings. *)

  end

end

