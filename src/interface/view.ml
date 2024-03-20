
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

    type _ t =
      | Signature : 'ty_var list * 'ty list * 'ty ->
          < ty_var : 'ty_var; ty : 'ty; .. > t (** Function signature *)
      (** View of type signatures. *)

  end

  module Ty = struct

    type _ t =
      | Var : 'ty_var ->
          < ty_var : 'ty_var; .. > t
        (**)
      | App : ('blt, 'ty_cst) head * 'ty list ->
          < ty_cst : 'ty_cst; builtin : 'blt; .. > t
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
      (** Let bindings *)
    (** First-order binders that can occur in terms. *)

    type _ pattern =
      | Var : 'term_var ->
          < term_var : 'term_var; .. > pattern
      | Constructor : 'term_cst * 'term_var list ->
          < term_var : 'term_var; term_cst : 'term_cst; .. > pattern (**)
    (** First-order patterns. *)

    type _ t =
      | Var : 'term_var ->
          < term_var : 'term_var; .. > t
        (** Term Variables*)
      | App : ('blt, 'term_cst) head * 'ty list * 'term list ->
          < ty : 'ty; term : 'term; term_cst : 'term_cst; builtin : 'blt; .. > t
        (** Polymorphic application of a function symbol, with explicit type arguments *)
      | Match :
          'term * (< term_var : 'term_var; term_cst : 'term_cst; .. > pattern * 'term) list ->
          < term_var : 'term_var; term_cst : 'term_cst; term : 'term; .. > t
        (** Pattern matching. *)
      | Binder :
          < ty_var : 'ty_var; term_var : 'term_var; term : 'term; .. > binder * 'term ->
          < ty_var : 'ty_var; term_var : 'term_var; term : 'term; .. > t
        (** Binders over a body term. *)
      (** View of terms in first-order. *)

  end


  (** The signature for a module that provides a first-order view of terms and
      types. *)
  module type S = sig

    type builtin
    (** builtin symbols *)

    module Ty : sig

      type t

      module Var : sig

        type t

      end

      module Cst : sig

        type t

        val arity : t -> int

      end

      exception Not_first_order of t
      (** exceptions raised by view functions on types that cannot be
          represented as first-order. *)

      val view : t ->
        < ty_var : Var.t; ty_cst : Cst.t; builtin : builtin; ty : t; .. > Ty.t
      (** View function for types.
          @raise Not_first_order_ty if the type cannot be viewed as first-order. *)

    end

    module Sig : sig

      type t

      val view : t ->
        < ty_var : Ty.Var.t; ty : Ty.t; .. > Sig.t

    end

    module Term : sig

      type t

      module Var : sig

        type t

        val ty : t -> Ty.t

      end

      module Cst : sig

        type t

        val ty : t -> Sig.t

      end

      exception Not_first_order of t
      (** exceptions raised by view functions on terms that cannot be
          represented as first-order. *)

      val view : t ->
        < ty_var: Ty.Var.t; term_var: Var.t; term_cst : Cst.t;
          builtin : builtin; ty : Ty.t; term : t; .. > Term.t
      (** View function for terms.
          @raise Not_first_order_term if the term cannot be viewed as first-order. *)

    end

  end

end
