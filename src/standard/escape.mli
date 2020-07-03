(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Escaping identifiers

    This module provides helpers to print identifers
    in languages, in order to ensure that the printed
    string is conform to a language specification, in
    order to avoid all kinds of injections (e.g.
    parentheses in identifier names, etc...) *)

(** {2 Identifier name manipulation} *)

val smap : (int -> char -> string) -> string -> string
(** [umap f] provides an equivalent of flat_map on strings.
    [f] is given the position of the character in the string (starting from [1]),
    and a character. *)

val rename : sep:char -> string -> string
(** A renaming function, which add an increasing number after the given separator. *)


(** {2 Identifier escaping} *)

module type Arg = Dolmen_intf.Id.Escape

module Make(Id : Arg) : sig

  type t
  (** The type of environnment/escaper for a given language.
      Identifiers printed using a given environment, are escaped, and
      its actual printed string recorded, in order to avoid future conflicts
      with other escaped identifiers. *)

  val mk :
    lang:string ->
    name:(Id.t -> string) ->
    escape:(string -> string) ->
    rename:(string -> string) -> t
  (** Create an escaper from scratch. The name function is called to determine
      the name of an identifier. The escape function is assumed
      to be idempotent and have a lot of fixpoints (i.e. all valid identifiers
      name should map to themselves) whereas the renaming function should
      have absolutely no fixpoint (i.e. for all strings [rename s <> s]) *)

  val flush : t -> unit
  (** Remove all the stored information about escaping and renaming. *)

  val print : t -> Format.formatter -> Id.t -> unit
  (** A printing function that automatically escapes and rename if needed to
      avoid name clashes, and then print to the given formatter. *)

end

