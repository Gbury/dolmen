
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

  val prop : t
  (** The type of propositions *)

  val of_var : Var.t -> t
  (** Create a type from a variable. *)

  val apply : Const.t -> t list -> t
  (** Application for types. *)

  val wildcard : unit -> t
  (** Create a fresh type wildcard. *)

  val tag : t -> 'a tag -> 'a -> unit
  (** Annotate the given type with the given tag and value. *)

end

(** Signature required by types for typing ae *)
module type Ae_Base = sig

  type t
  (** The type of types. *)

  val bool : t
  (** The type of booleans *)

  val unit : t
  (** Unit type *)

end


(** Signature required by types for typing tptp *)
module type Ae_Arith = sig

  type t
  (** The type of types. *)

  val int : t
  (** The type of integers. *)

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

(** Signature required for types for typing smtlib real_int arithmetic. *)
module type Smtlib_Real_Int = sig

  include Smtlib_Int
  include Smtlib_Real with type t := t

  val equal : t -> t -> bool
  (** Equality function on types. *)

end

(** Signature required for types for typing smtlib arrays *)
module type Smtlib_Array = sig

  type t
  (** The type of types *)

  val array : t -> t -> t
  (** The type of functionnal arrays from one type to another. *)

end

(** Signature required for types for typing smtlib bitvectors *)
module type Smtlib_Bitv = sig

  type t
  (** The type of types *)

  val bitv : int -> t
  (** Create a fixed size bitvector type. *)

end
