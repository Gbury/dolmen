
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** Escpaing identifiers

    This module provides helpers to print identifers
    in languages that restricts the range of acceptable characters
    (for instance HTML, Coq, ...) *)


(** {2 Environment for escaping} *)

type t
(** The type of environnment/escaper for a given language.
    Identifiers printed using a given environment, are escaped, and
    its actual printed string recorded, in order to avoid future conflicts
    with other escaped identifiers. *)

val id : t -> Format.formatter -> Id.t -> unit
(** Printer for archsat identifiers. *)

(** {3 Identifier names} *)

type name =
  | Exact of string   (** The given name is to be printed exactly as is *)
  | Normal of string  (** The given name should be escaped/renamed if necessary *)
(** Variant type to specify the name (and status) of how an identifier should be
    printed. Typically, id name that come from the input problem should be escaped,
    while names declared inside the source code can ask to be exact (for instance,
    qualified names using module paths should *not* be escaped, etc...) *)

(** Custom environments *)

val mk :
  lang:string ->
  name:(Id.t -> name) ->
  escape:(string -> string) ->
  rename:(string -> string) -> t
(** Create an escaper from scratch. The name function is called to determine
    the name of an identifier. The escape function is assumed
    to be idempotent and have a lot of fixpoints (i.e. all valid identifiers
    name should map to themselves) whereas the renaming function should
    have absolutely no fixpoint (i.e. for all strings [rename s <> s]) *)

val rename : sep:char -> string -> string
(** A renaming function, which adds an increasing number after
    the last occurrence of the given separator. *)

val umap :
  (int -> Uchar.t option -> Uchar.t list) ->
  string -> string
(** [umap f] provides an equivalent of flat_map on unicode strings.
    [f] is given the position of the character in the string (starting from [1]),
    and a unicode character (or [None] if decoding failed for that byte). *)

val pp_uchar : Format.formatter -> Uchar.t -> unit
(** Prints an uchar *)


