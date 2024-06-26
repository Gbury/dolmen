
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Typechecker state} *)

module T : Dolmen_type.Thf.S
  with type 'a Tag.t = 'a Dolmen.Std.Tag.t
   and type Ty.t = Dolmen.Std.Expr.ty
   and type Ty.Var.t = Dolmen.Std.Expr.ty_var
   and type Ty.Const.t = Dolmen.Std.Expr.ty_cst
   and type T.t = Dolmen.Std.Expr.term
   and type T.Var.t = Dolmen.Std.Expr.term_var
   and type T.Const.t = Dolmen.Std.Expr.term_cst
   and type T.Cstr.t = Dolmen.Std.Expr.term_cst
(** The raw type-checker module. *)


type ty_state
(** The type of state used by the {!Make} functor. *)

val new_state : unit -> ty_state
(** Generate a fresh typing state. *)

val typer_state : ty_state -> T.state
(** Return the underlying typer state. *)


(* {2 Warnings} *)

val forbidden_arith_expr : (Dolmen_type.Arith.Smtlib2.config * string) Report.Error.t
(** Error for arithmetic expressions that do not respect the specification (e.g.
    non-linear expressions, expressions not in difference logic, etc...). *)

val bad_arith_expr : (Dolmen_type.Arith.Smtlib2.config * string) Report.Warning.t
(** Warning for arithmetic expressions that do not strictly conform to the
    specification (e.g. linear arithmetic), but are close enough that they
    should probably be accepted. *)


val unknown_logic : string Report.Warning.t
(** Unknown logic warning *)

module Ext : sig
  (** Define typing extensions.

      These extensions are typically extensions used by some community,
      but not yet part of the standard.

      @since 0.10 *)

  type t
  (** The type of typing extensions. *)

  val name : t -> string
  (** Extension name, sould be suitable for cli options. *)

  val builtins : t -> Typer_intf.lang -> T.builtin_symbols
  (** Reutnrs the typing builtins from an extension. *)

  val create :
    name:string -> builtins:(Typer_intf.lang -> T.builtin_symbols) -> t
  (** Create a new extension. *)

  val bvconv : t
  (** Typing extension to add `bv2nat` and `int2bv`. *)

  val find_all : string -> t list
  (** Returns the extensions that have been registered with the given name.

      @since 0.11 *)
end

(** {2 Typechecker Functor} *)

module type Typer_Full = Typer_intf.Typer_Full

module Typer(State : State.S)
  : Typer_Full with type state = State.t
                and type 'a key := 'a State.key
                and type ty_state = ty_state
                and type env = T.env
                and type 'a fragment = 'a T.fragment
                and type error = T.error
                and type warning = T.warning
                and type builtin_symbols = T.builtin_symbols
                and type extension = Ext.t

(** {2 Typechecker Pipe} *)

module type Typer = Typer_intf.Typer

module type S = Typer_intf.S

module Make
    (Expr : Expr_intf.S)
    (Print : Expr_intf.Print
     with type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type ty_def := Expr.ty_def
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
    (State : State.S)
    (Typer : Typer
     with type state := State.t
      and type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type ty_def := Expr.ty_def
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
  : S with type state := State.t
       and type env = Typer.env
       and type 'a key := 'a State.key
       and type ty := Expr.ty
       and type ty_var := Expr.ty_var
       and type ty_cst := Expr.ty_cst
       and type ty_def := Expr.ty_def
       and type term := Expr.term
       and type term_var := Expr.term_var
       and type term_cst := Expr.term_cst
       and type formula := Expr.formula
