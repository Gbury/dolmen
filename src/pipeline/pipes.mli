(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Top-level operations

    This module defines top-level operators, i.e functions to act
    on top-level statements. *)

module Make
    (Opt : Options.S)
    (C : Callback.S)
    (Expr : Expr.S)
    (Typer : Typer.S)
    (Solver : Solver.S)
    (Proof : Proof.S)
    (Model : Model.S) : sig

  (** {2 Types} *)

  type 'a tr_stmt = {
    contents : 'a;
    implicit : Proof.id list;
  }
  (* Used for wrapping translated contents with implicit declarations *)

  type +'a stmt = {
    id          : Dolmen.Id.t;
    contents    : 'a;
    loc         : Dolmen.ParseLocation.t option;
  }
  (** Wrapper around statements. It records implicit type declarations. *)

  type executed = [
    | `Executed
  ]

  type type_decls = [
    | `Type_decl of Expr.ty_const
    | `Term_decl of Expr.term_const
  ]
  (** The type of top-level type declarations. *)

  type decl = [
      `Decl of Proof.id tr_stmt
  ]
  (** The type of proof id declaration. *)

  type type_defs = [
    | `Type_def of Dolmen.Id.t * Expr.ty_var list * Expr.ty
    | `Term_def of Dolmen.Id.t * Expr.ty_var list * Expr.term_var list * Expr.term
  ]
  (** The type of top-level type definitions. *)

  type def = [
    | `Def of (Dolmen.Id.t * Proof.term) tr_stmt
  ]
  (** The type of id definition. *)

  type assume = [
    | `Hyp of Expr.formula
    | `Goal of Expr.formula
    | `Clause of Expr.formula list
  ]
  (** The type of top-level assertion statements *)

  type solve_sequent = [
    | `Left of Solver.id * Expr.formula
    | `Right of Solver.id * Expr.formula
  ]
  (** The type of sequent components (for proof output). *)

  type proof_sequent = [
    | `Left of Proof.id tr_stmt
    | `Right of (Solver.id * Proof.id) tr_stmt
  ]

  type solve = [
    | `Solve of Expr.formula list
  ]
  (** Top-level solve instruction *)

  type result = [
    | `Skipped
    | `Unknown
    | `Proof of Proof.t
    | `Model of Model.t
  ]
  (** The type of results for a solve instruction. *)

  type typechecked = [ executed | type_defs | type_decls | assume | solve ]
  (** The type of statements after typechecking *)

  type solved      = [ executed | type_defs | type_decls | solve_sequent | result ]
  (** The type of solved statement *)

  type translated  = [ executed | decl | def | proof_sequent | result ]
  (** The type of translated statements *)


  (** {2 Pipes} *)

  val parse :
    Dolmen.Statement.t list -> Opt.t ->
    Opt.t * (Opt.t -> Dolmen.Statement.t option)
  (** Parsing function. Reads a list of prelude statements, and the input options and
      returns a tuple of the new options (including the detected input language),
      together with a statement generator. *)

  val execute : Opt.t * Dolmen.Statement.t ->
    [ `Continue of Opt.t * Dolmen.Statement.t | `Done of Opt.t ]
    (** Perform side effects of statement (such as the 'exit' statement. *)
(*
val expand : Opt.t * Dolmen.Statement.t ->
  Opt.t * [ `Ok | `Gen of bool * Dolmen.Statement.t Gen.gen ]
(** Expand statements (such as includes). Returns the new options, and either:
    - [ `Ok ], which means the statement can be propagated as is
    - [ `Gen (flat, g) ], if the statement expands into a generator [g]. The bool [flat]
      indicates wether the statements in [g] should be treated as a single group of
      statements (with regards to timeouts, etc...), or as a list of independant statements
      (each with its own timeout...).
*)

val run_typecheck : Opt.t -> bool
(** Should the typechecker be run ? *)

val typecheck : Opt.t * Dolmen.Statement.t -> typechecked stmt
(** Typechecks a statement. *)

val solve : Opt.t * typechecked stmt -> solved stmt
(** Solve a statement *)

val print_res : Opt.t * solved stmt -> unit
(** Print the results of solved statements *)

val run_translate : Opt.t -> bool
(** Should translation (and subsequent passes) be run ? *)

val translate : Opt.t * solved stmt -> translated stmt
(** Translate statements into proof statements *)

val export : Opt.t * translated stmt -> unit
(** Export various information; usually for debugging purposes. *)

val print_proof : Opt.t * translated stmt -> unit
(** Print the proof according to the options *)

val print_model : Opt.t * translated stmt -> unit
(** Print the proof according to the options *)
*)
end
