
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Printing of identifiers *)
(* ************************************************************************* *)

exception Cannot_print of string
(** Exception raised when some input cannot be printed due to lexical
    conventions. In that case, the string contains a message explaining
    why the printing failed. *)

val id : Format.formatter -> Dolmen_std.Name.t -> unit
(** Print an identifier, quoting it if necessary. *)
