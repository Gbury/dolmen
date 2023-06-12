(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(* Small signature to define a number of types. *)
module type Types = sig

  type state
  type ty
  type ty_var
  type ty_cst
  type ty_def

  type term
  type term_var
  type term_cst

  type formula

  type env

end

(** This modules defines a wrapper around the bare-bones typechecker
    provided by Dolmen_type. It provides convenience function to match
    on Dolmen untyped statements and type-check them. *)
module type Typer = sig

  include Types

  type input = [
    | `Logic of Logic.language State.file
    | `Response of Response.language State.file
  ]

  type lang = [
    | `Logic of Logic.language
    | `Response of Response.language
  ]

  val reset :
    state -> ?loc:Dolmen.Std.Loc.t -> unit -> state

  val reset_assertions :
    state -> ?loc:Dolmen.Std.Loc.t -> unit -> state

  val push :
    state -> input:input -> ?loc:Dolmen.Std.Loc.t -> int -> state

  val pop :
    state -> input:input -> ?loc:Dolmen.Std.Loc.t -> int -> state

  val set_logic :
    state -> input:input -> ?loc:Dolmen.Std.Loc.t ->
    string -> state * Dolmen_type.Logic.t

  val defs :
    mode:[`Create_id | `Use_declared_id] ->
    state -> input:input -> ?loc:Dolmen.Std.Loc.t ->
    ?attrs:Dolmen.Std.Term.t list -> Dolmen.Std.Statement.defs ->
    state * [
     | `Type_alias of Dolmen.Std.Id.t * ty_cst * ty_var list * ty
     | `Term_def of Dolmen.Std.Id.t * term_cst * ty_var list * term_var list * term
     | `Instanceof of Dolmen.Std.Id.t * term_cst * ty list * ty_var list * term_var list * term
    ] list

  val decls :
    state -> input:input -> ?loc:Dolmen.Std.Loc.t ->
    ?attrs:Dolmen.Std.Term.t list -> Dolmen.Std.Statement.decls ->
    state * [
      | `Type_decl of ty_cst * ty_def option
      | `Term_decl of term_cst
    ] list

  val terms :
    state -> input:input -> ?loc:Dolmen.Std.Loc.t ->
    ?attrs:Dolmen.Std.Term.t list -> Dolmen.Std.Term.t list ->
    state * term list

  val formula :
    state -> input:input -> ?loc:Dolmen.Std.Loc.t ->
    ?attrs:Dolmen.Std.Term.t list -> goal:bool -> Dolmen.Std.Term.t ->
    state * formula

  val formulas :
    state -> input:input -> ?loc:Dolmen.Std.Loc.t ->
    ?attrs:Dolmen.Std.Term.t list -> Dolmen.Std.Term.t list ->
    state * formula list

  val typing_wrap :
    ?attrs:Dolmen.Std.Term.t list ->
    ?loc:Dolmen.Std.Loc.t ->
    input:input -> state -> f:(env -> 'a) -> state * 'a

end

(** Extended signature for typer. *)
module type Typer_Full = sig

  type state
  (** Global state for the while pipeline. *)

  type 'a key
  (** Type of keys for the state. *)

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
  (** The type of builin symbols for the type-checker. *)

  val ty_state : ty_state key
  (** Key to store the local typechecking state in the global pipeline state. *)

  val check_model : bool key
  (** The typechecker needs to know whether we are checking models or not. *)

  val smtlib2_forced_logic : string option key
  (** Force the typechecker to use the given logic (instead of using the one declared
      in the `set-logic` statement). *)

  include Typer
    with type env := env
     and type state := state
     and type ty := Dolmen.Std.Expr.ty
     and type ty_var := Dolmen.Std.Expr.ty_var
     and type ty_cst := Dolmen.Std.Expr.ty_cst
     and type ty_def := Dolmen.Std.Expr.ty_def
     and type term := Dolmen.Std.Expr.term
     and type term_var := Dolmen.Std.Expr.term_var
     and type term_cst := Dolmen.Std.Expr.term_cst
     and type formula := Dolmen.Std.Expr.formula
  (** This signature includes the requirements to instantiate the {Pipes.Make:
      functor*)

  val init :
    ?ty_state:ty_state ->
    ?smtlib2_forced_logic:string option ->
    ?additional_builtins:(state -> lang -> builtin_symbols) ->
    state -> state

  val additional_builtins : (state -> lang -> builtin_symbols) key
  (** Add new builtin symbols to the typechecker, depending on the current
      language.

      {b Note.} The additional builtins are never used for Dimacs and iCNF.

      @before 0.9 [additional_builtins] had type [builtin_symbols ref]. *)

  val report_error : input:input -> state -> error -> state
  (** Report a typing error by calling the appropriate state function. *)

  val report_warning : input:input -> state -> warning -> state
  (** Return a typing warning by calling the appropriate state function. *)

  val pop_inferred_model_constants : state -> Dolmen.Std.Expr.term_cst list
  (** TODO:doc *)

end

(** This modules defines the result signature of the {Typer.Pipe} functor *)
module type S = sig

  include Types

  type 'a key

  val type_check : bool key

  val init :
    type_check:bool -> state -> state

  (** {2 Types} *)

  type +'a stmt = {
    id          : Dolmen.Std.Id.t;
    loc         : Dolmen.Std.Loc.t;
    contents    : 'a;
  }
  (** Wrapper around statements. It records implicit type declarations. *)

  type decl = [
    | `Type_decl of ty_cst * ty_def option
    | `Term_decl of term_cst
  ]
  (** The type of top-level type declarations. *)

  type decls = [
    | `Decls of decl list
  ]
  (** A list of type declarations. *)

  type def = [
    | `Type_alias of Dolmen.Std.Id.t * ty_cst * ty_var list * ty
    | `Term_def of Dolmen.Std.Id.t * term_cst * ty_var list * term_var list * term
    | `Instanceof of Dolmen.Std.Id.t * term_cst * ty list * ty_var list * term_var list * term
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
    | `Solve of formula list * formula list
    (** [`Solve (hyps, goals)] represents a sequent with local hypotheses [hyps]
        and local goals [goals]. *)
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
    | `Set_logic of string * Dolmen_type.Logic.t
    | `Set_info of Dolmen.Std.Statement.term
    | `Set_option of Dolmen.Std.Statement.term
  ]
  (** Info setters *)

  type stack_control = [
    | `Pop of int
    | `Push of int
    | `Reset_assertions
    | `Reset
  ]
  (** Stack control *)

  type exit = [
    | `Exit
  ]
  (** Exit statement *)

  type typechecked = [ defs | decls | assume | solve | get_info | set_info | stack_control | exit ]
  (** The type of statements after typechecking *)

  val print : Format.formatter -> typechecked stmt -> unit
  (** Printing funciton for typechecked statements. *)

  val check : state -> Dolmen.Std.Statement.t -> state * typechecked stmt
  (** Typechecks a statement. *)

  val typecheck : state -> Dolmen.Std.Statement.t ->
    state * [ `Continue of typechecked stmt | `Done of unit ]
  (** Typechecks a statement. *)

end

