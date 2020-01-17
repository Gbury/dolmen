
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

module T : Dolmen_type.Tff.S with type 'a Tag.t = 'a Dolmen.Tag.t
                              and type Ty.t = Dolmen.Expr.ty
                              and type Ty.Var.t = Dolmen.Expr.ty_var
                              and type Ty.Const.t = Dolmen.Expr.ty_const
                              and type T.t = Dolmen.Expr.term
                              and type T.Var.t = Dolmen.Expr.term_var
                              and type T.Const.t = Dolmen.Expr.term_const
                              and type T.Cstr.t = Dolmen.Expr.term_const

include Dolmen_loop.Typer.S with type state := State.t
                             and type ty := Dolmen.Expr.ty
                             and type ty_var := Dolmen.Expr.ty_var
                             and type ty_const := Dolmen.Expr.ty_const
                             and type term := Dolmen.Expr.term
                             and type term_var := Dolmen.Expr.term_var
                             and type term_const := Dolmen.Expr.term_const
                             and type formula := Dolmen.Expr.formula


val report_error : Format.formatter -> T.err -> unit
(** Report a typing error on the given formatter. *)

val print_shadowing_reasons :
  Format.formatter ->
  (Dolmen.Id.t * Dolmen_type.Tff.reason * Dolmen_type.Tff.reason) ->
  unit

