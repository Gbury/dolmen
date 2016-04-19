
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Generic type for parsers *)

module type S = sig

  type token

  type statement

  exception Error

  val file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> statement list

end

