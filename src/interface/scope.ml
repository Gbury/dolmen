
(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

type _ id =
  | Ty_var : 'ty_var -> < ty_var : 'ty_var; .. > id
  | Ty_cst : 'ty_cst -> < ty_cst : 'ty_cst; .. > id
  | Term_var : 'term_var -> < term_var : 'term_var; .. > id
  | Term_cst : 'term_cst -> < term_cst : 'term_cst; .. > id (**)
(** A useful type to merge together 4 types of identifiers, corresponding
    to types/terms variables/constants. *)

module type S = sig

  type t
  (** The type of scope for a given language. *)

  type id
  (** The type for identifiers used in expressions to be printed. *)

  type name
  (** The type of printable names. *)

  type rename
  (** The type of renaming scheme that is used. *)

  type on_conflict =
    | Error
    | Shadow
    | Rename (**)
  (** The different behaviours that are available upon encountering a conflict
      beetween names for identifiers. *)

  val mk_rename : 'acc -> ('acc -> name -> 'acc * name) -> rename
  (** Create a renaming scheme. *)

  val empty :
    rename:rename ->
    sanitize:(id -> name -> name) ->
    on_conflict:(prev_id:id -> new_id:id -> name:name -> on_conflict) ->
    t
  (** Create an escaper from scratch. The name function is called to determine
      the name of an identifier. The escape function is assumed
      to be idempotent and have a lot of fixpoints (i.e. all valid identifiers
      name should map to themselves) whereas the renaming function should
      have absolutely no fixpoint (i.e. for all strings [rename s <> s]) *)

  val bind : t -> id -> t
  (** Bind a new id in the scope. *)

  val name : t -> id -> name
  (** Return the name bound to an id. *)

end
