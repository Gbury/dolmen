(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Typer *)

module type Pipe_types = sig

  type state

  type ty
  type ty_var
  type ty_const

  type term
  type term_var
  type term_const

  type formula

end

(** This modules defines the smallest signatures for a typechecker that allow
    to instantiate the {Typer.Pipe} functor. *)
module type Pipe_arg = sig

  include Pipe_types

  val typecheck :
    state -> bool

  val reset :
    state -> ?loc:Dolmen.Std.Loc.t -> unit -> state

  val push :
    state -> ?loc:Dolmen.Std.Loc.t -> int -> state

  val pop :
    state -> ?loc:Dolmen.Std.Loc.t -> int -> state

  val set_logic :
    state -> ?loc:Dolmen.Std.Loc.t -> string -> state

  val defs :
    state -> ?loc:Dolmen.Std.Loc.t ->
    ?attr:Dolmen.Std.Term.t -> Dolmen.Std.Statement.defs ->
    state * [
     | `Type_def of Dolmen.Std.Id.t * ty_var list * ty
     | `Term_def of Dolmen.Std.Id.t * term_const * ty_var list * term_var list * term
    ] list

  val decls :
    state -> ?loc:Dolmen.Std.Loc.t ->
    ?attr:Dolmen.Std.Term.t -> Dolmen.Std.Statement.decls ->
    state * [
      | `Type_decl of ty_const
      | `Term_decl of term_const
    ] list

  val terms :
    state -> ?loc:Dolmen.Std.Loc.t ->
    ?attr:Dolmen.Std.Term.t -> Dolmen.Std.Term.t list ->
    state * term list

  val formula :
    state -> ?loc:Dolmen.Std.Loc.t ->
    ?attr:Dolmen.Std.Term.t -> goal:bool -> Dolmen.Std.Term.t ->
    state * formula

  val formulas :
    state -> ?loc:Dolmen.Std.Loc.t ->
    ?attr:Dolmen.Std.Term.t -> Dolmen.Std.Term.t list ->
    state * formula list

end

(** This modules defines the result signature of the {Typer.Make} functor *)
module type S = sig

  type state
  (** The type of state for a whole pipeline. *)

  type ty_state
  (** The type for the state of the typer. *)

  type env
  (** The type of the typechecker environment. *)

  type 'a fragment
  (** The type of fragments on which error/warning can occur. *)

  type error
  (** The type of type-checking errors. *)

  type warning
  (** The type of type-checking warnings. *)

  type builtin_symbols
  (** The type of builint symbols for the type-checker. *)

  include Pipe_arg
    with type state := state
     and type ty := Dolmen.Std.Expr.ty
     and type ty_var := Dolmen.Std.Expr.ty_var
     and type ty_const := Dolmen.Std.Expr.ty_const
     and type term := Dolmen.Std.Expr.term
     and type term_var := Dolmen.Std.Expr.term_var
     and type term_const := Dolmen.Std.Expr.term_const
     and type formula := Dolmen.Std.Expr.formula
  (** This signature includes the requirements to instantiate the {Pipes.Make:
      functor*)

  val print_fragment : Format.formatter -> env * 'a fragment -> unit
  (** Print a code fragment *)

  val report_error : Format.formatter -> error -> unit
  (** Report a typing error on the given formatter. *)

  val report_warning : warning ->
    (Format.formatter -> unit -> unit) option
  (** Return a reporter for the given warning, if the warning should be
      reported. *)

  val additional_builtins : builtin_symbols ref
  (** This reference can be modified to parse new builtin symbols. By default no
      additional builtin symbols are parsed. It is added for all the languages
      except Dimacs, and iCNF. *)

end

(** This modules defines the result signature of the {Typer.Pipe} functor *)
module type Pipe_res = sig

  include Pipe_types

  (** {2 Types} *)

  type +'a stmt = {
    id          : Dolmen.Std.Id.t;
    loc         : Dolmen.Std.Loc.t;
    contents    : 'a;
  }
  (** Wrapper around statements. It records implicit type declarations. *)

  type decl = [
    | `Type_decl of ty_const
    | `Term_decl of term_const
  ]
  (** The type of top-level type declarations. *)

  type decls = [
    | `Decls of decl list
  ]
  (** A list of type declarations. *)

  type def = [
    | `Type_def of Dolmen.Std.Id.t * ty_var list * ty
    | `Term_def of Dolmen.Std.Id.t * term_const * ty_var list * term_var list * term
  ]
  (** The type of top-level type definitions. Type definitions are inlined and so can be ignored. *)

  type defs = [
    | `Defs of def list
  ]
  (** A list of definitions *)

  type assume = [
    | `Hyp of formula
    | `Goal of formula
    | `Clause of formula list
  ]
  (** The type of top-level assertion statements *)

  type solve = [
    | `Solve of formula list
  ]
  (** Top-level solve instruction *)

  type get_info = [
    | `Get_info of string
    | `Get_option of string
    | `Get_proof
    | `Get_unsat_core
    | `Get_unsat_assumptions
    | `Get_model
    | `Get_value of term list
    | `Get_assignment
    | `Get_assertions
    | `Echo of string
    | `Plain of Dolmen.Std.Statement.term
  ]
  (** Various info getters *)

  type set_info = [
    | `Set_logic of string
    | `Set_info of Dolmen.Std.Statement.term
    | `Set_option of Dolmen.Std.Statement.term
  ]
  (** Info setters *)

  type stack_control = [
    | `Pop of int
    | `Push of int
    | `Reset_assertions
    | `Reset
    | `Exit
  ]
  (** Stack control *)

  type typechecked = [ defs | decls | assume | solve | get_info | set_info | stack_control ]
  (** The type of statements after typechecking *)

  val typecheck : state -> Dolmen.Std.Statement.t ->
    state * [ `Continue of typechecked stmt | `Done of unit ]
  (** Typechecks a statement. *)

end

