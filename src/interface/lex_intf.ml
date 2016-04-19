
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Generic type for lexers *)

module type S = sig

  type token

  exception Error

  val token : Lexing.lexbuf -> token

end

