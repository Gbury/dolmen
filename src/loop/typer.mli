
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Typechecker state} *)

type ty_state
(** The type of state used by the {!Make} functor. *)

val new_state : unit -> ty_state
(** Generate a fresh typing state.
    The bool argument given decides whether locations are kept (provides
      better error messages, but uses more memory when running). *)

module T : Dolmen_type.Tff.S
  with type 'a Tag.t = 'a Dolmen.Std.Tag.t
   and type Ty.t = Dolmen.Std.Expr.ty
   and type Ty.Var.t = Dolmen.Std.Expr.ty_var
   and type Ty.Const.t = Dolmen.Std.Expr.ty_cst
   and type T.t = Dolmen.Std.Expr.term
   and type T.Var.t = Dolmen.Std.Expr.term_var
   and type T.Const.t = Dolmen.Std.Expr.term_cst
   and type T.Cstr.t = Dolmen.Std.Expr.term_cst
(** The raw type-checker module. *)


(** {2 Typechecker Functor} *)

module type S = Typer_intf.S

module Make
    (S : State_intf.Typer with type ty_state := ty_state)
  : S with type state := S.t
       and type ty_state := ty_state
       and type env := T.env
       and type 'a fragment := 'a T.fragment
       and type error := T.error
       and type warning := T.warning
       and type builtin_symbols := T.builtin_symbols

(** {2 Typechecker Pipe} *)

module type Pipe_arg = Typer_intf.Pipe_arg
module type Pipe_res = Typer_intf.Pipe_res

module Pipe
    (Expr : Expr_intf.S)
    (Print : Expr_intf.Print
     with type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
    (State : State_intf.Typer_pipe)
    (Typer : Pipe_arg
     with type state := State.t
      and type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
  : Pipe_res
    with type state := State.t
     and type ty := Expr.ty
     and type ty_var := Expr.ty_var
     and type ty_cst := Expr.ty_cst
     and type term := Expr.term
     and type term_var := Expr.term_var
     and type term_cst := Expr.term_cst
     and type formula := Expr.formula


