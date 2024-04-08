(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Expressions

    This modules defines the smallest signatures for expressions that allow
    to isntantiates the {Pipes.Make} functor. *)

module type S = sig

  type ty
  type ty_var
  type ty_cst
  type ty_def

  type term
  type term_var
  type term_cst

  type formula

end

module type Print = sig

  include S

  val ty : Format.formatter -> ty -> unit
  val ty_var : Format.formatter -> ty_var -> unit
  val ty_cst : Format.formatter -> ty_cst -> unit
  val ty_def : Format.formatter -> ty_def -> unit

  val term : Format.formatter -> term -> unit
  val term_var : Format.formatter -> term_var -> unit
  val term_cst : Format.formatter -> term_cst -> unit

  val formula : Format.formatter -> formula -> unit

end

module type Export = sig

  include S

  module Ty : sig
    type t = ty
    module Var : sig
      include Dolmen_intf.Id.Scope
      with type t = ty_var
       and type path := Dolmen_std.Path.t
    end
    module Const : sig
      include Dolmen_intf.Id.Scope
      with type t = ty_cst
       and type path := Dolmen_std.Path.t
    end
  end

  module Term : sig
    type t = term

    val neg : t -> t
    (** Logical negation. *)

    val _or : t list -> t
    (** Disjunction. *)

    val of_cst : term_cst -> t
    (** Create a formula out of a constant. *)

    module Var : sig
      include Dolmen_intf.Id.Scope
      with type t = term_var
       and type path := Dolmen_std.Path.t
    end

    module Const : sig
      include Dolmen_intf.Id.Scope
      with type t = term_cst
       and type path := Dolmen_std.Path.t

      val mk : Dolmen_std.Path.t -> ty -> t
      (** Create a constant symbol *)
    end
  end

end

