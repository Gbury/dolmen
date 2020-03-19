
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Interface for Dolmen parsers. *)

module type S = sig

  (** {2 Main interface} *)

  type token
  (** The type of token consumed by the parser. *)

  type statement
  (** The type of top-level declarations returned by the parser. *)

  exception Error
  (** Exception raised by the parser when it encounters an error. *)

  val file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> statement list
  (** A function that parses an entire file, i.e until the end-of-file token,
      and return the list of parsed statements. *)

  val input : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> statement option
  (** A function to parse a single statement. Reutnrs [None] if it encounters
      the end-of-file token. Used for incremental parsing. *)


  (** {2 Menhir incremental interface} *)

  module MenhirInterpreter : sig
    include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
      with type token = token
  end

  module Incremental : sig
    val file : Lexing.position -> (statement list) MenhirInterpreter.checkpoint
    val input : Lexing.position -> (statement option) MenhirInterpreter.checkpoint
  end

end

