
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

val consume :
  newline:(Lexing.lexbuf -> unit) ->
  sync:(Lexing.lexbuf -> unit) ->
  Lexing.lexbuf -> unit
(** Consumes all characters on the current line. *)

