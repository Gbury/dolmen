
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for Types
    This module defines Interfaces that implementation of types must
    respect in order to be used to instantiate functors. *)


(** {2 Signature for Typecheked types} *)

module type Tff = sig

  (** Signature required by types for typing first-order
      polymorphic terms. *)


  (** {4 Abstract types} *)

  type t
  (** The type of types. *)

  type 'a tag
  (** A type for tags to attach to arbitrary types. *)

  val hash : t -> int
  (** A hash function for types, should be suitable to create hashtables. *)

  val equal : t -> t -> bool
  (** An equality function on types. Should be compatible with the hash function. *)

  (** A module for variables that occur in types. *)
  module Var : sig

    type t
    (** The type of variables the can occur in types *)

    val hash : t -> int
    (** A hash function for type variables, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on type variables. Should be compatible with the hash function. *)

    val compare : t -> t -> int
    (** Comparison function on variables. *)

    val mk : string -> t
    (** Create a new type variable with the given name. *)

  end

  (** A module for constant symbols the occur in types. *)
  module Const : sig

    type t
    (** The type of constant symbols the can occur in types *)

    val hash : t -> int
    (** A hash function for type constants, should be suitable to create hashtables. *)

    val equal : t -> t -> bool
    (** An equality function on type constants. Should be compatible with the hash function. *)

    val arity : t -> int
    (** Return the arity of the given symbol. *)

    val mk : string -> int -> t
    (** Create a type constant with the given arity. *)

    val tag : t -> 'a tag -> 'a -> unit
    (** Tag a variable. *)

  end

  val of_var : Var.t -> t
  (** Create a type from a variable. *)

  val apply : Const.t -> t list -> t
  (** Application for types. *)

  val tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given type with the given tag and value. *)


  (** {4 Concrete types} *)

  val prop : t
  (** The type of propositions *)

  (** Algebraic types, aka sum types, aka datatypes *)
  module Adt : sig

    type cstr
    (** An algebraic type constructor. Note that such constructors are used to
        build terms, and not types, e.g. consider the following:
        [type 'a list = Nil | Cons of 'a * 'a t], then [Nil] and [Cons] are the
        constructors, while [list] would the a constant of arity 1 used to
        name the type. *)

    val cstr : string -> cstr
    (** Create a constructor from its name. It can later be bound to a specific
        type when the `define` function is called. *)

    type descr = (cstr * t list) list
    (** A description of an algebraic type, which is a list of cases.
        A case of an algebraic type in the context of a type definition. Each case
        contain a constructors and its associated arguments' types.
        In the exmaple of lists, we can distinguish two cases:
        - [Nil], which is a constructor with no arguments
        - [Cons of 'a * 'a list], which is a constructor with two arguments. These two
          arguments can contain type variables as well as concrete types. *)

  end

  val define_adt : t -> Adt.descr -> unit
  (** Define the given type with the associated defintion description.
      TODO: specify the raised exception for multiple definitions. *)

  val inspect : t -> [
      | `Abstract
      | `Algebraic of Adt.descr
    ]
  (** Inspect the definition of a given type. Useful for isntance for deciding
      completeness of pattern matching, or legality of field access for records. *)

end

