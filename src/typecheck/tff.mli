
(** Typechecking of standard terms
    This module provides functions to typecheck terms from the
    untyped syntax tree defined in the standard implementation. *)

module type S = Tff_intf.S
(** Typechecker external interface *)

module Make
    (Tag: Dolmen.Intf.Tag.S)
    (Ty: Dolmen.Intf.Ty.Tff
     with type 'a tag := 'a Tag.t)
    (T: Dolmen.Intf.Term.Tff
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type 'a tag := 'a Tag.t)
  : S with module Tag = Tag
       and module Ty = Ty
       and module T = T

