
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 First-order View} *)

module FO = struct
  (** This explicits a view of terms as polymorphic first-order
      types and terms. In strict first-order, the distinction between
      types and terms is clear and useful enough that the view
      distinguishes the two. *)

  type ('cst, 'blt) head =
    | Cst of 'cst
    | Builtin of 'blt (**)
  (** The type of application heads. *)

  module Sig = struct

    type _ view =
      | Signature : 'ty_var list * 'ty list * 'ty ->
          < ty_var : 'ty_var; ty : 'ty; .. > view (** Function signature *)
      (** View of type signatures. *)

  end

  module Ty = struct

    type _ view =
      | Var : 'ty_var ->
          < ty_var : 'ty_var; .. > view
        (**)
      | App : ('ty_cst, 'blt) head * 'ty list ->
          < ty_cst : 'ty_cst; builtin : 'blt; ty : 'ty; .. > view
        (**)
      (** View of types in first-order. In first-order, types, which include basically
          variables and application of type constructors to types, are differentiated
          from type signatures, which include function types. *)

  end

  module Term = struct

    type _ binder =
      | Exists : 'ty_var list * 'term_var list ->
          < ty_var : 'ty_var; term_var : 'term_var; .. > binder
      (** Existancial quantification *)
      | Forall : 'ty_var list * 'term_var list ->
          < ty_var : 'ty_var; term_var : 'term_var; .. > binder
      (** Universal quantification *)
      | Letin : ('term_var * 'term) list ->
          < term_var : 'term_var; term : 'term; .. > binder
      (** Sequential Let bindings *)
      | Letand : ('term_var * 'term) list ->
          < term_var : 'term_var; term : 'term; .. > binder
      (** Parallel Let bindings *)
    (** First-order binders that can occur in terms. *)

    type _ pattern =
      | Var : 'term_var ->
          < term_var : 'term_var; .. > pattern
      | Constructor : 'term_cst * 'term_var list ->
          < term_var : 'term_var; term_cst : 'term_cst; .. > pattern (**)
    (** First-order patterns. *)

    type _ view =
      | Var : 'term_var ->
          < term_var : 'term_var; .. > view
        (** Term Variables*)
      | App : ('term_cst, 'blt) head * 'ty list * 'term list ->
          < ty : 'ty; term : 'term; term_cst : 'term_cst; builtin : 'blt; .. > view
        (** Polymorphic application of a function symbol, with explicit type arguments *)
      | Match :
          'term * ('t pattern * 'term) list ->
          (< term_var : 'term_var; term_cst : 'term_cst; term : 'term; .. > as 't) view
        (** Pattern matching. *)
      | Binder :
          < ty_var : 'ty_var; term_var : 'term_var; term : 'term; .. > binder * 'term ->
          < ty_var : 'ty_var; term_var : 'term_var; term : 'term; .. > view
        (** Binders over a body term. *)
      (** View of terms in first-order. *)

  end

  (* Aliases to avoid shadowing in the module type S below *)
  module Vty = Ty
  module VT = Term

  (** The signature for a module that provides a first-order view of terms and
      types. *)
  module type S = sig

    type ty
    type ty_var
    type ty_cst
    type term
    type term_var
    type term_cst
    type formula

    type builtin = <
      ty : ty;
      ty_var : ty_var;
      ty_cst : ty_cst;
      term : term;
      term_var : term_var;
      term_cst : term_cst;
    > Builtin.t
    (** builtin symbols *)

    module Ty : sig

      type t = ty

      module Var : sig

        type t = ty_var

      end

      module Cst : sig

        type t = ty_cst

        val arity : t -> int

      end

      exception Not_first_order of t
      (** exceptions raised by view functions on types that cannot be
          represented as first-order. *)

      val view : t ->
        < ty_var : Var.t; ty_cst : Cst.t; builtin : builtin; ty : t; .. > Ty.view
      (** View function for types.
          @raise Not_first_order_ty if the type cannot be viewed as first-order. *)

    end

    module Sig : sig

      type t

      val view : t ->
        < ty_var : Ty.Var.t; ty : Ty.t; .. > Sig.view

    end

    module Term : sig

      type t = term

      module Var : sig

        type t = term_var

        val ty : t -> Ty.t

      end

      module Cst : sig

        type t = term_cst

        val ty : t -> Sig.t

      end

      exception Not_first_order of t
      (** exceptions raised by view functions on terms that cannot be
          represented as first-order. *)

      val ty : t -> Ty.t
      (** Return the type of a term. *)

      val view : t ->
        < ty_var: Ty.Var.t; term_var: Var.t; term_cst : Cst.t;
          builtin : builtin; ty : Ty.t; term : t; .. > Term.view
      (** View function for terms.
          @raise Not_first_order_term if the term cannot be viewed as first-order. *)

    end

    module Formula : sig

      type t = formula

      val ty : t -> Ty.t
      (** Return the type of a formula. *)

      val view : t ->
        < ty_var: Ty.Var.t; term_var: Term.Var.t; term_cst : Term.Cst.t;
          builtin : builtin; ty : Ty.t; term : Term.t; .. > VT.view
      (** View function for terms.
          @raise Not_first_order_term if the term cannot be viewed as first-order. *)

    end

  end

end
