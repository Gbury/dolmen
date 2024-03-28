
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type Print_id = sig

  type id
  type env
  type name

  val bind : env -> id -> env
  val name : env -> id -> name

end

module type Print = sig

  type name

  type ty
  type ty_var
  type ty_cst
  type term
  type term_var
  type term_cst

  type t

  type 'a key

  val get : t -> 'a key -> 'a option
  val set : t -> 'a key -> 'a -> t

  module Ty_var : Print_id with type env := t and type id := ty_var and type name := name
  module Ty_cst : Print_id with type env := t and type id := ty_cst and type name := name
  module Term_var : Print_id with type env := t and type id := term_var and type name := name
  module Term_cst : Print_id with type env := t and type id := term_cst and type name := name

end
