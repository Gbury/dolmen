(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Typer *)

(** This modules defines the smallest signatures for a typechecker that allow
    to instantiate the {State.Make} functor. *)
module type T = sig

  type ty_state

end

(** This modules defines the smallest signatures for a typechecker that allow
    to instantiate the {Pipes.Make} functor. *)
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
     | `Term_def of Dolmen.Id.t * term_const * ty_var list * term_var list * term
    ] *
    (Dolmen.ParseLocation.t * string) list

  val decls :
    state -> ?attr:Dolmen.Term.t ->
    Dolmen.Statement.decl list ->
    state * [
      | `Type_decl of ty_const
      | `Term_decl of term_const
    ] list *
    (Dolmen.ParseLocation.t * string) list

  val terms :
    state -> ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    state * term list * (Dolmen.ParseLocation.t * string) list

  val formula :
    state -> ?attr:Dolmen.Term.t -> goal:bool -> Dolmen.Term.t ->
    state * formula * (Dolmen.ParseLocation.t * string) list

  val formulas :
    state -> ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    state * formula list * (Dolmen.ParseLocation.t * string) list

end
