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
