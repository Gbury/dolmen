(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Typer

    This modules defines the smallest signatures for a typechecker that allow
    to isntantiates the {Pipes.Make} functor. *)

module type S = sig

  type state

  type ty
  type ty_var
  type ty_const

  type term
  type term_var
  type term_const

  type formula

  val typecheck : state -> bool

  val def :
    state -> ?attr:Dolmen.Term.t -> Dolmen.Id.t -> Dolmen.Term.t ->
    state *
    [
     | `Type_def of Dolmen.Id.t * ty_var list * ty
     | `Term_def of Dolmen.Id.t * ty_var list * term_var list * term
     ]

  val decl :
    state -> ?attr:Dolmen.Term.t -> Dolmen.Id.t -> Dolmen.Term.t ->
    state *
    [
     | `Type_decl of ty_const
     | `Term_decl of term_const
     ]

  val inductives :
    state -> ?attr:Dolmen.Term.t -> Dolmen.Statement.inductive list ->
    state * ty_const list

  val terms :
    state -> ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    state * term list

  val formula :
    state -> ?attr:Dolmen.Term.t -> goal:bool -> Dolmen.Term.t ->
    state * formula

  val formulas :
    state -> ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    state * formula list

end
