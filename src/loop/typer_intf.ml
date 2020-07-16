(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Typer *)

(** This modules defines the smallest signatures for a typechecker that allow
    to instantiate the {State.Make} functor. *)
module type T = sig
  type type_st
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

  val set_logic :
    state -> ?loc:Dolmen.ParseLocation.t -> string -> state

  val defs :
    state -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Statement.defs ->
    state * [
     | `Type_def of Dolmen.Id.t * ty_var list * ty
     | `Term_def of Dolmen.Id.t * term_const * ty_var list * term_var list * term
    ] list

  val decls :
    state -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Statement.decls ->
    state * [
      | `Type_decl of ty_const
      | `Term_decl of term_const
    ] list

  val terms :
    state -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    state * term list

  val formula :
    state -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> goal:bool -> Dolmen.Term.t ->
    state * formula

  val formulas :
    state -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    state * formula list

end
