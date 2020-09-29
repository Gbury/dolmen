
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Interface for Dolmen lexers. *)

module type S = sig

  type token
  (** The type of token produced by the lexer. *)

  exception Error
  (** The exception raised by the lexer when it cannot produce a token. *)

  val descr : token -> Tok.descr
  (** Asspcoate a description to each token, to help with error messages. *)

  val token : (Lexing.lexbuf -> unit) -> Lexing.lexbuf -> token
  (** The function producing token from a lexbuf. *)

end

