
(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** {1 Escaping identifiers}

    This module provides helpers to print identifers in languages.
    There are two properties that this module tries to enforce:

   - ensure that the printed identifier conforms to the language specification,
     to ensure it is parsed correctly (e.g. avoid forbidden characters,
     reserved names, etc..)
   - ensure that identifiers are correctly scoped, and that when printed,
     they refer to the correct identifier. Consider for isntance the
     (legally correct) typed term `forall x/0 x/1. x/0 = x/1`, where
     both variable have the same name `x`: one of them need to be renamed,
     or the term cannot be printed.
*)

(** {2 Identifier escaping} *)

type 'a id = 'a Dolmen_intf.Scope.id =
  | Ty_var : 'ty_var -> < ty_var : 'ty_var; .. > id
  | Ty_cst : 'ty_cst -> < ty_cst : 'ty_cst; .. > id
  | Term_var : 'term_var -> < term_var : 'term_var; .. > id
  | Term_cst : 'term_cst -> < term_cst : 'term_cst; .. > id

module type S = Dolmen_intf.Scope.S
  with type name := Name.t
module type Arg = Dolmen_intf.Id.Scope_Full
  with type path := Path.t
   and type namespace := Namespace.t

module Wrap
    (Ty_var : Arg)(Ty_cst : Arg)
    (Term_var : Arg)(Term_cst : Arg)
  : Arg with type t = <
      ty_var : Ty_var.t;
      ty_cst : Ty_cst.t;
      term_var :  Term_var.t;
      term_cst : Term_cst.t
    > id

module Make(Id : Arg) : S with type id := Id.t
