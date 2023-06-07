
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

val almost_linear : string Report.Warning.t
(** Almost linear warning. *)

val unknown_logic : string Report.Warning.t
(** Unknown logic warning *)


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

