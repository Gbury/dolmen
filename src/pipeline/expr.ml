(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Expressions

    This modules defines the smallest signatures for expressions that allow
    to isntantiates the {Pipes.Make} functor. *)

module type S = sig

  type ty
  type ty_var
  type ty_const

  type term
  type term_var
  type term_const

  type formula

end
