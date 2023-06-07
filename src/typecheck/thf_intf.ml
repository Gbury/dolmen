
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** External Typechecker interface for THF

    This module defines the external typechcker interface, that is,
    the interface of an instantiated typechecker. *)

(** {1 Typechecker interface} *)

(** Typechecker interface *)
module type S = sig

  (** {2 Module aliases} *)
  module Tag: Dolmen.Intf.Tag.S
  module Ty: Dolmen.Intf.Ty.Thf
    with type 'a tag := 'a Tag.t
     and type path := Dolmen.Std.Path.t
  module T: Dolmen.Intf.Term.Thf
    with type ty := Ty.t
     and type ty_var := Ty.Var.t
     and type ty_const := Ty.Const.t
     and type ty_def := Ty.def
     and type 'a tag := 'a Tag.t
     and type path := Dolmen.Std.Path.t

  include Intf.Formulas
    with type ty := Ty.t
     and type ty_var := Ty.Var.t
     and type ty_cst := Ty.Const.t
     and type ty_def := Ty.def
     and type term := T.t
     and type term_var := T.Var.t
     and type term_cst := T.Const.t
     and type term_cstr := T.Cstr.t
     and type term_field := T.Field.t
     and type 'a ast_tag := 'a Tag.t

end
