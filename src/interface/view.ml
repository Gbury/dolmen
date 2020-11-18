
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interface for views
    This module defines Interfaces for viewing functions, that can be
    used to inspect terms. Such views are useful to abstract over the
    concrete implementation of terms. *)


(** {2 Interface for types} *)

module Ty = struct
  (** This view represents types, with distinguished cases for
      types wich have builtin semantics. *)

  type ('var, 'cst, 'blt, 'ty) t = [
    | `Int
    (** The type of integers *)
    | `Rat
    (** The type of rationals *)
    | `Real
    (** The type of reals *)
    | `Array of 'ty * 'ty
    (** The type of arrays from a source type to a destination type. *)
    | `Bitv of int
    (** Bitvectors of the given length. *)
    | `Float of int * int
    (** [`Float (e, s)] is the type of floats whose exponent have size [e],
        and significand have size [s] (hidden bit included). *)
    | `String
    (** The type of strings *)
    | `String_reg_lang
    (** The type of regular languages over strings *)
    | `Var of 'var
    (** Type variables *)
    | `App of 'blt * 'cst * 'ty list
    (** Application of type constructors. *)
    | `Sig of ('var list * 'ty list * 'ty)
    (** Function signatures *)
    | `Other of 'ty
    (* Any construction that doesn't fit in those above. *)
  ]

  (** The signature for a module that provides a view of types as
      specified above. *)
  module type S = sig

    type var
    (** The type of variables *)

    type cst
    (** The type of constants *)

    type blt
    (** The type of builtins *)

    type ty
    (** The type of types *)

    val ty : ty -> (var, cst, blt, ty) t
    (** View function *)

  end

end


(** {2 First-order View} *)

module FO = struct
  (** This explicits a view of terms as polymorphic first-order
      types and terms. In strict first-order, the distinction between
      types and terms is clear and useful enough that the view
      distinguishes the two. *)

  type ('ty_var, 'term_var, 'term) binder =
    | Exists of 'ty_var list * 'term_var list (** Existancial quantification *)
    | Forall of 'ty_var list * 'term_var list (** Universal quantification *)
    | Letin of ('term_var * 'term) list       (** Let bindings *)
  (** First-order binders that can occur in terms. *)

  type ('ty_var, 'ty_cst, 'blt, 'ty) ty_view =
    | Var of 'ty_var
    | App of 'blt * 'ty_cst * 'ty list (**)
  (** View of types in first-order. In first-order, types, which include basically
      variables and applicaiton of type constructors to types, are differentiated
      from type signatures, which include function types.
      In first-order, a function type would be represented by a triplet
      ['ty_var list * 'ty list * 'ty], of type variables the signature is quantified
      over, a list of argument types, and a return type. *)

  type ('ty_var, 'term_var, 'term_cst, 'builtin, 'ty, 'term) term_view =
    | Var of 'term_var
    (** Term Variables*)
    | App of 'builtin * 'term_cst * 'ty list * 'term list
    (** Polymorphic application of a function symbol, with explicit type arguments *)
    | Match of 'term * ('term * 'term) list
    (** Pattern matching. *)
    | Binder of ('ty_var, 'term_var, 'term) binder * 'term
    (** Binders over a body term. *)
  (** View of terms in first-order. *)

  (** The signature for a module that provides a first-oreder view of terms and
      types. *)
  module type S = sig

    type ty_var
    (** type variables *)

    type ty_cst
    (** type constructors *)

    type term_var
    (** term variables *)

    type term_cst
    (** function symbols *)

    type builtin
    (** builtin symbols *)

    type ty
    (** types *)

    type term
    (** terms *)

    exception Not_first_order_ty of ty
    (** exceptions raised by view functions on types that cannot be
        represented a first-order terms. *)

    exception Not_first_order_term of term
    (** exceptions raised by view functions on terms that cannot be
        represented a first-order terms. *)

    val ty : ty -> (ty_var, ty_cst, builtin, ty) ty_view
    (** View function for types.
        @raise Not_first_order_ty if the type cannot be viewed as first-order. *)

    val term : term -> (ty_var, term_var, term_cst, builtin, ty, term) term_view
    (** View function for terms.
        @raise Not_first_order_term if the term cannot be viewed as first-order. *)

  end

end

(** {2 Higher-order view} *)

module HO = struct
  (** This explicits a view of terms as polymorphic higher-order terms.
      For higher-order, the distinciton between types and terms is less
      pertinent, so the view here is meant to work for both types and terms.
      This makes it so that this view can even represent dependently-typed
      terms if needed. *)

  type ('var, 'term) binder =
    | Pi of 'var list               (** Type variable quantification for function types *)
    | Arrow of 'term list           (** Function type (arguments are "bound") *)
    | Exists of 'var list           (** Existencial quantification *)
    | Forall of 'var list           (** Universal quantification *)
    | Letin of ('var * 'term) list  (** Let-binding *)
  (** The binder for higher-order terms *)

  type ('var, 'cst, 'blt, 'term) t =
    | Var of 'var
    (** Variables *)
    | Cst of 'blt * 'cst
    (** Constants *)
    | App of 'term * 'term list
    (** Applications *)
    | Binder of ('var, 'term) binder * 'term
    (** Binders *)
    | Match of 'term * ('term * 'term) list
    (** Pattern matching *)
  (** View for higher-order terms*)

  (** The signature for a module that provides a higher-order view of terms. *)
  module type S = sig

    type var
    (** variables *)

    type cst
    (** constants *)

    type blt
    (** builtins *)

    type term
    (** terms *)

    exception Not_higher_order of term
    (** exceptions raised by view functions on terms that cannot be
        represented a higher-order terms. *)

    val term : term -> (var, cst, blt, term) t
    (** View function
        @raise Not_higher_order if the term cannot be viewed as higher-order. *)

  end

end
