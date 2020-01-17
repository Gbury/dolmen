(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Top-level operations

    This module defines top-level operators, i.e functions to act
    on top-level statements. *)

module Make
    (Expr : Expr.S)
    (State : State.S
     with type term := Expr.term)
    (Typer : Typer.S
     with type state := State.t
      and type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_const := Expr.ty_const
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_const := Expr.term_const
      and type formula := Expr.formula)
  : sig

  (** {2 Types} *)

  type +'a stmt = {
    id          : Dolmen.Id.t;
    contents    : 'a;
    loc         : Dolmen.ParseLocation.t option;
  }
  (** Wrapper around statements. It records implicit type declarations. *)

  type executed = [
    | `Executed
  ]

  type decls = [
    | `Inductives of Expr.ty_const list
    | `Type_decl of Expr.ty_const
    | `Term_decl of Expr.term_const
  ]
  (** The type of top-level type declarations. *)

  type defs = [
    | `Type_def of Dolmen.Id.t * Expr.ty_var list * Expr.ty
    | `Term_def of Dolmen.Id.t * Expr.ty_var list * Expr.term_var list * Expr.term
  ]
  (** The type of top-level type definitions. *)

  type assume = [
    | `Hyp of Expr.formula
    | `Goal of Expr.formula
    | `Clause of Expr.formula list
  ]
  (** The type of top-level assertion statements *)

  type solve = [
    | `Solve of Expr.formula list
  ]
  (** Top-level solve instruction *)

  type typechecked = [ executed | defs | decls | assume | solve ]
  (** The type of statements after typechecking *)


  (** {2 Pipes} *)

  val parse :
    Dolmen.Statement.t list -> State.t ->
    State.t * (State.t -> Dolmen.Statement.t option)
  (** Parsing function. Reads a list of prelude statements, and the state and
      returns a tuple of the new state (including the detected input language),
      together with a statement generator. *)

  val expand : State.t * Dolmen.Statement.t ->
    State.t * [ `Ok | `Gen of bool * Dolmen.Statement.t Gen.gen ]
  (** Expand statements (such as includes). Returns the new state, and either:
      - [ `Ok ], which means the statement can be propagated as is
      - [ `Gen (flat, g) ], if the statement expands into a generator [g]. The bool [flat]
        indicates wether the statements in [g] should be treated as a single group of
        statements (with regards to timeouts, etc...), or as a list of independant statements
        (each with its own timeout...). *)

  val typecheck : State.t * Dolmen.Statement.t ->
    [ `Continue of (State.t * typechecked stmt) | `Done of State.t ]
  (** Typechecks a statement. *)

end
