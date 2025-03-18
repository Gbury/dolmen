
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for Types
    This module defines Interfaces that implementation of types must
    respect in order to be used to instantiate functors. *)


(** {2 Signature for Typecheked types} *)

(** Signature required by types for typing first-order
    polymorphic terms. *)
module type Tff = sig

  type t
  (** The type of types. *)

  type def
  (** The type of type definitions *)

  type path
  (** The type of paths to constants. *)

  type 'a tag
  (** A type for tags to attach to arbitrary types. *)

  val print : Format.formatter -> t -> unit
  (** Printing function. *)

  exception Prenex_polymorphism of t
  (** Raised when the type provided is polymorphic, but occurred in a
      place where polymorphic types are forbidden by prenex/rank-1
      polymorphism. *)

  (** A module for variables that occur in types. *)
  module Var : sig

    type t
    (** The type of variables the can occur in types *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val print : Format.formatter -> t -> unit
    (** Printing function. *)

    val mk : string -> t
    (** Create a new type variable with the given name. *)

    val wildcard : unit -> t
    (** Create a fresh type wildcard. *)

    val is_wildcard : t -> bool
    (** Is the variable a type wildcard ? *)

    val set_tag : t -> 'a tag -> 'a -> unit
    (** Set the value bound to a tag. *)

    val get_tag : t -> 'a tag -> 'a option
    (** Return the value bound to a tag (if any). *)

    val add_tag : t -> 'a list tag -> 'a -> unit
    (** Add a value to the list of values bound to a tag. *)

    val get_tag_list : t -> 'a list tag -> 'a list
    (** Returns all the values tagged on a variable. *)

    val unset_tag : t -> _ tag -> unit
    (** Remove the binding to a tag. *)

  end

  (** A module for constant symbols the occur in types. *)
  module Const : sig

    type t
    (** The type of constant symbols the can occur in types *)

    val compare : t -> t -> int
    (** Comparison function on type constants. *)

    val print : Format.formatter -> t -> unit
    (** Printing function. *)

    val arity : t -> int
    (** Return the arity of the given symbol. *)

    val mk : path -> int -> t
    (** Create a type constant with the given arity. *)

    val set_tag : t -> 'a tag -> 'a -> unit
    (** Set the value bound to a tag. *)

    val add_tag : t -> 'a list tag -> 'a -> unit
    (** Add a value to the list of values bound to a tag. *)

  end

  val equal : t -> t -> bool
  (** Test equality of types. *)

  val prop : t
  (** The type of propositions *)

  val of_var : Var.t -> t
  (** Create a type from a variable. *)

  val apply : Const.t -> t list -> t
  (** Application for types. *)

  val arrow : t list -> t -> t
  (** Create an arrow type. *)

  val pi : Var.t list -> t -> t
  (** Create a polymorphic type. *)

  val fv : t -> Var.t list
  (** Returns the list of free_variables in the type. *)

  val set_wildcard : Var.t -> t -> unit
  (** Set a wildcard. *)

  val add_wildcard_hook : hook:(Var.t -> t -> unit) -> Var.t -> unit
  (** Add a hook to a wildcard, the hook will be run *)

  val set_tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given type with the given tag and value. *)

  val add_tag : t -> 'a list tag -> 'a -> unit
  (** Add a value to the list of values bound to a tag. *)

  type view = private [>
    | `Wildcard of Var.t
    | `Arrow of t list * t
    | `Pi of Var.t list * t
  ]
  (** Partial views for types. *)

  val view : t -> view
  (** Partial view of a type. *)

  val freshen : t -> t
  (** Replaces all bound variables in a type, ensuring that the returned type
      contains only fresh bound type variables. *)

  val instance_of : t -> t -> t list option
  (** [instance_of poly t] decides whether [t] is an instance of [poly],
      that is whether there are some types [l] such that a term of
      type [poly] applied to type arguments [l] gives a term of type
      [t]. *)

end

module type Thf = sig

  include Tff

  val arrow : t list -> t -> t
  (** Create a function type. *)

  val pi : Var.t list -> t -> t
  (** Create a rank-1/prenex polymorphc type. *)

end

(** Signature required by types for typing ae *)
module type Ae_Base = sig

  type t
  (** The type of types. *)

  val bool : t
  (** The type of booleans *)

  val unit : t
  (** Unit type *)

  val int : t
  (** The type of integers *)

  val real : t
  (** The type of reals *)

end

(** Signature required by types for typing ae's arithmetic *)
module type Ae_Arith = sig

  type t
  (** The type of types *)

  val int : t
  (** The type of integers *)

  val real : t
  (** The type of reals *)

  type view = private [>
    | `Int
    | `Real
  ]
  (** Partial view for types. *)

  val view : t -> view
  (** Partial view of a type. *)

end

(** Signature required by types for typing ae arrays *)
module type Ae_Array = sig

  type t
  (** The type of types *)

  val int : t
  (** The type of integers, used as a default type of indexes
      when no type is provided *)

  val array : t -> t -> t
  (** The type of functionnal arrays from one type to another. *)

end

(** Signature required by types for typing ae's bitvectors *)
module type Ae_Bitv = sig

  type t
  (** The type of types *)

  val bitv : int -> t
  (** Create a fixed size bitvector type. *)

end

(** Signature required by types for typing ae's floats *)
module type Ae_Float = sig

  type t
  (** The type of types *)

  val roundingMode: t
  (** Type of the rounding modes *)

end

(** Signature required by types for typing tptp *)
module type Tptp_Base = sig

    type t
    (** The type of types *)

    val prop : t
    (** The type of propositions. *)

    val base : t
    (** An arbitrary base type. *)

end

(** Signature required by types for typing tptp *)
module type Tptp_Arith = sig

    type t
    (** The type of types *)

    val int : t
    (** The type of integers *)

    val rat : t
    (** The type of rationals *)

    val real : t
    (** The type of reals *)

    val equal : t -> t -> bool
    (** Equality on types. *)

end



(** Signature required by types for typing smtlib core theory *)
module type Smtlib_Base = sig

  type t
  (** The type of type constants (i.e. type constructors) *)

  val prop : t
  (** The type constructor of propositions. *)

end

(** Signature required by types for typing smtlib integer arithmetic *)
module type Smtlib_Int = sig

  type t
  (** The type of types *)

  val int : t
  (** The type for integer expressions. *)

end

(** Signature required by types for typing smtlib real arithmetic *)
module type Smtlib_Real = sig

  type t
  (** The type of types *)

  val real : t
  (** The type for integer expressions. *)

end

(** Signature required by types for typing smtlib real_int arithmetic. *)
module type Smtlib_Real_Int = sig

  include Smtlib_Int
  include Smtlib_Real with type t := t

  type view = private [>
    | `Int
    | `Real
  ]
  (** Partial view for types. These are used by the Reals_Ints theory
      to perform type-based dispatch, and automatic conversion of Ints
      to Reals when specified by the specification. *)

  val view : t -> view
  (** Partial view of a type. *)

end

(** Signature required by types for typing smtlib arrays *)
module type Smtlib_Array = sig

  type t
  (** The type of types *)

  val array : t -> t -> t
  (** The type of functionnal arrays from one type to another. *)

  type view = private [>
    | `Int
    | `Real
    | `Bitv of int
    | `Array of t * t
  ]
  (** Partial views for types. These are used in the Array theory
      to enforce some restrictions logics impose on the types of
      arrays that cna occur. *)

  val view : t -> view
  (** Partial view of a type. *)

end

(** Signature required by types for typing smtlib bitvectors *)
module type Smtlib_Bitv = sig

  type t
  (** The type of types *)

  val bitv : int -> t
  (** Create a fixed size bitvector type. *)

  type view =  private [>
    | `Bitv of int
  ]
(** Partial views for types. These are used in the bitv theory
    to check invariant on indexes of bitv operations (e.g.
    check that the indexes of an extract do not go out of bounds). *)

  val view : t -> view
  (** Partial view of a type. *)

end

(** Signature required by types for typing smtlib bitvectors *)
module type Smtlib_Float = sig

  type t
  (** The type of types *)

  val bitv : int -> t
  (** Create a fixed size bitvector type. *)

  val float : int -> int -> t
  (** Create a float type with fixed exponent size in bits and fixed significand,
      including the hidden bit. *)

  val roundingMode: t
  (** Type of the rounding modes *)

  type view = private [>
    | `Real
    | `Bitv of int
    | `Float of int * int
  ]
  (** Partial views for types. These are used in the Float theory to
      perform type-base dispatch for some conversion functions. *)

  val view : t -> view
  (** Partial view of a type. *)

end

(* Signature required by types for typing the smtlib string theory *)
module type Smtlib_String = sig

  type t
  (** The type of types *)

  val int : t
  (** The type of ints *)

  val string : t
  (** The type of strings *)

  val string_reg_lang : t
  (** The type of regular languages over strings *)

end

(** Signature required by types for typing tptp *)
module type Zf_Base = sig

    type t
    (** The type of types *)

    val prop : t
    (** The type of propositions. *)

end

(** Signature required by types for typing tptp *)
module type Zf_Arith = sig

    type t
    (** The type of types *)

    val int : t
    (** The type of integers *)

end
