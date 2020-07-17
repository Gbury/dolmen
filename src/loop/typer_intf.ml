(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Typer *)


(** This modules defines the smallest signatures for a typechecker that allow
    to instantiate the {Pipes.Make} functor. *)
module type Pipes = sig

  type t

  type ty
  type ty_var
  type ty_const

  type term
  type term_var
  type term_const

  type formula

  val typecheck : t -> bool

  val set_logic :
    t -> ?loc:Dolmen.ParseLocation.t -> string -> t

  val defs :
    t -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Statement.defs ->
    t * [
     | `Type_def of Dolmen.Id.t * ty_var list * ty
     | `Term_def of Dolmen.Id.t * term_const * ty_var list * term_var list * term
    ] list

  val decls :
    t -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Statement.decls ->
    t * [
      | `Type_decl of ty_const
      | `Term_decl of term_const
    ] list

  val terms :
    t -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    t * term list

  val formula :
    t -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> goal:bool -> Dolmen.Term.t ->
    t * formula

  val formulas :
    t -> ?loc:Dolmen.ParseLocation.t ->
    ?attr:Dolmen.Term.t -> Dolmen.Term.t list ->
    t * formula list

end

(** This modules defines the result signature of the {Typer.Make} functor *)
module type S = sig

  type t
  (** The type of state for a whole pipeline. *)

  type ty_state
  (** The type for the state of the typer. *)

  type 'a fragment
  (** The type of fragments on which error/warning can occur. *)

  type error
  (** The type of type-checing errors. *)

  type warning
  (** The type of type-checking warnings. *)

  type builtin_symbols
  (** The type of builint symbols for the type-checker. *)

  include Pipes
    with type t := t
     and type ty := Dolmen.Expr.ty
     and type ty_var := Dolmen.Expr.ty_var
     and type ty_const := Dolmen.Expr.ty_const
     and type term := Dolmen.Expr.term
     and type term_var := Dolmen.Expr.term_var
     and type term_const := Dolmen.Expr.term_const
     and type formula := Dolmen.Expr.formula
  (** This signature includes the requirements to instantiate the {Pipes.Make:
      functor*)

  val print_fragment : Format.formatter -> 'a fragment -> unit
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
