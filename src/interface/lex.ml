
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Interface for Dolmen lexers. *)

module type S = sig

  type token
  (** The type of token produced by the lexer. *)

  exception Error
  (** The exception raised by the lexer when it cannot produce a token. *)

  val token : Lexing.lexbuf -> token
  (** The function producing token from a lexbuf. *)

end

