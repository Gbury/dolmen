
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = sig

  (** {2 Main interface} *)

  type type_st
  type solve_st

  module T : Dolmen_type.Tff_intf.S
    with type 'a Tag.t = 'a Dolmen.Tag.t
     and type Ty.t = Dolmen.Expr.ty
     and type Ty.Var.t = Dolmen.Expr.ty_var
     and type Ty.Const.t = Dolmen.Expr.ty_const
     and type T.t = Dolmen.Expr.term
     and type T.Var.t = Dolmen.Expr.term_var
     and type T.Const.t = Dolmen.Expr.term_const
     and type T.Cstr.t = Dolmen.Expr.term_const

  include Typer_intf.S
    with type state := (Parser.language, type_st, solve_st) Dolmen.State.state
     and type ty := Dolmen.Expr.ty
     and type ty_var := Dolmen.Expr.ty_var
     and type ty_const := Dolmen.Expr.ty_const
     and type term := Dolmen.Expr.term
     and type term_var := Dolmen.Expr.term_var
     and type term_const := Dolmen.Expr.term_const
     and type formula := Dolmen.Expr.formula

  val new_state : unit -> type_st
  (* Generate a fresh typing state. *)

  val report_error : Format.formatter -> T.error -> unit
  (** Report a typing error on the given formatter. *)

  val report_warning : T.warning ->
    (Format.formatter -> unit -> unit) option
  (** Return a reporter for the given warning, if the warning should be
      reported. *)

end

module Make(S : State_intf.Typer) : S with type solve_st := S.solve_st

