
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Typechecker state} *)

type ty_state
(** The type of state used by the {!Make} functor. *)

val new_state : unit -> ty_state
(** Generate a fresh typing state. *)

module T : Dolmen_type.Tff.S
  with type 'a Tag.t = 'a Dolmen.Tag.t
   and type Ty.t = Dolmen.Expr.ty
   and type Ty.Var.t = Dolmen.Expr.ty_var
   and type Ty.Const.t = Dolmen.Expr.ty_const
   and type T.t = Dolmen.Expr.term
   and type T.Var.t = Dolmen.Expr.term_var
   and type T.Const.t = Dolmen.Expr.term_const
   and type T.Cstr.t = Dolmen.Expr.term_const
(** The raw type-checker module. *)


(** {2 Typechecker Functor} *)

module type S = Typer_intf.S

module Make
    (S : State_intf.Typer with type ty_state := ty_state)
  : S with type t := S.t
       and type ty_state := ty_state
       and type 'a fragment := 'a T.fragment
       and type error := T.error
       and type warning := T.warning
       and type builtin_symbols := T.builtin_symbols

