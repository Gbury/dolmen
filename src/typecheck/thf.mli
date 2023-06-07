
(** Typechecking of standard terms
    This module provides functions to typecheck terms from the
    untyped syntax tree defined in the standard implementation. *)

module type S = Thf_intf.S
(** Typechecker external interface *)

module Make
    (Tag: Dolmen.Intf.Tag.S)
    (Ty: Dolmen.Intf.Ty.Thf
     with type 'a tag := 'a Tag.t
      and type path := Dolmen.Std.Path.t)
    (T: Dolmen.Intf.Term.Thf
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type ty_def := Ty.def
      and type 'a tag := 'a Tag.t
      and type path := Dolmen.Std.Path.t)
  : S with module Tag = Tag
       and module Ty = Ty
       and module T = T
