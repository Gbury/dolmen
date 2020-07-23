(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Top-level operations

    This module defines top-level operators, i.e functions to act
    on top-level statements. *)

module Make
    (Expr : Expr_intf.S)
    (State : State_intf.Pipes
     with type term := Expr.term)
    (Typer : Typer_intf.Pipes
     with type t := State.t
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
    id          : Dolmen.Std.Id.t;
    contents    : 'a;
    loc         : Dolmen.Std.ParseLocation.t option;
  }
  (** Wrapper around statements. It records implicit type declarations. *)

  type decl = [
    | `Type_decl of Expr.ty_const
    | `Term_decl of Expr.term_const
  ]
  (** The type of top-level type declarations. *)

  type decls = [
    | `Decls of decl list
  ]
  (** A list of type declarations. *)

  type def = [
    | `Type_def of Dolmen.Std.Id.t * Expr.ty_var list * Expr.ty
    | `Term_def of Dolmen.Std.Id.t * Expr.term_const * Expr.ty_var list * Expr.term_var list * Expr.term
  ]
  (** The type of top-level type definitions. Type definitions are inlined and so can be ignored. *)

  type defs = [
    | `Defs of def list
  ]
  (** A list of definitions *)

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

  type get_info = [
    | `Get_info of string
    | `Get_option of string
    | `Get_proof
    | `Get_unsat_core
    | `Get_unsat_assumptions
    | `Get_model
    | `Get_value of Expr.term list
    | `Get_assignment
    | `Get_assertions
    | `Echo of string
    | `Plain of Dolmen.Std.Statement.term
  ]

  type set_info = [
    | `Set_logic of string
    | `Set_info of Dolmen.Std.Statement.term
    | `Set_option of Dolmen.Std.Statement.term
  ]

  type stack_control = [
    | `Pop of int
    | `Push of int
    | `Reset_assertions
    | `Reset
    | `Exit
  ]

  type typechecked = [ defs | decls | assume | solve | get_info | set_info | stack_control ]
  (** The type of statements after typechecking *)


  (** {2 Pipes} *)

  val parse :
    Dolmen.Std.Statement.t list -> State.t ->
    State.t * (State.t -> Dolmen.Std.Statement.t option)
  (** Parsing function. Reads a list of prelude statements, and the state and
      returns a tuple of the new state (including the detected input language),
      together with a statement generator. *)

  val expand : State.t * Dolmen.Std.Statement.t ->
    State.t * [ `Ok | `Gen of bool * Dolmen.Std.Statement.t Gen.t ]
  (** Expand statements (such as includes). Returns the new state, and either:
      - [ `Ok ], which means the statement can be propagated as is
      - [ `Gen (flat, g) ], if the statement expands into a generator [g]. The bool [flat]
        indicates wether the statements in [g] should be treated as a single group of
        statements (with regards to timeouts, etc...), or as a list of independant statements
        (each with its own timeout...). *)

  val typecheck : State.t * Dolmen.Std.Statement.t ->
    [ `Continue of (State.t * typechecked stmt) | `Done of State.t ]
  (** Typechecks a statement. *)

end
