
(** Typechecking of standard terms
    This module provides functions to typecheck terms from the
    untyped syntax tree defined in the standard implementation. *)

module type S = Tff_intf.S
(** Typechecker external interface *)

type reason =
  | Inferred of Dolmen.ParseLocation.t
  | Declared of Dolmen.ParseLocation.t (**)
(** The type of reasons for constant typing *)

type ('ty_const, 'term_cstr, 'term_field, 'term_const) binding = [
  | `Not_found
  | `Ty of 'ty_const * reason
  | `Cstr of 'term_cstr * reason
  | `Term of 'term_const * reason
  | `Field of 'term_field * reason
]
(** The bindings that can occur inside the typechecker. *)

(** Module type to define various warning functions that may
    be called during typechecking. *)
module type Warn = sig

  type ty
  type ty_var
  type ty_const

  type term
  type term_var
  type term_cstr
  type term_const
  type term_field

  val shadow : Dolmen.Id.t ->
    (ty_const, term_cstr, term_field, term_const) binding ->
    (ty_const, term_cstr, term_field, term_const) binding ->
    unit

  val unused_ty_var : Dolmen.ParseLocation.t -> ty_var -> unit
  val unused_term_var : Dolmen.ParseLocation.t -> term_var -> unit

  val error_in_attribute : Dolmen.ParseLocation.t -> exn -> unit

  val not_found : Dolmen.Id.t -> (int -> Dolmen.Id.t list) -> unit

  val superfluous_destructor :
    Dolmen.ParseLocation.t -> Dolmen.Id.t -> Dolmen.Id.t -> term_const -> unit

end

module Make
    (Tag: Dolmen_intf.Tag.S)
    (Ty: Dolmen_intf.Type.Tff
     with type 'a tag := 'a Tag.t)
    (T: Dolmen_intf.Term.Tff
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type 'a tag := 'a Tag.t)
    (W : Warn
     with type ty := Ty.t
      and type ty_var := Ty.Var.t
      and type ty_const := Ty.Const.t
      and type term := T.t
      and type term_var := T.Var.t
      and type term_cstr := T.Cstr.t
      and type term_const := T.Const.t
      and type term_field := T.Field.t
    )
  : S with module Tag = Tag
       and module Ty = Ty
       and module T = T

