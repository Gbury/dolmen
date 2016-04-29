
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Interface for Dolmen languages modules *)

module type S = sig

  type token
  (** The type of tokens produced by the language lexer. *)

  type statement
  (** The type of top-level directives recognised by the parser. *)

  module Lexer : Lex_intf.S
    with type token := token
  (** The Lexer module for the language. *)

  module Parser : Parse_intf.S
    with type token := token
     and type statement := statement
  (** The Parser module for the language. *)

  val parse_file : string -> statement list
  (** Parse the given file *)

  val parse_input :
    [ `Stdin | `File of string ] -> (unit -> statement option)
  (** incremental parsing. Given an input to read (either a file, or stdin),
      returns a generator that will incrementally parse the statements.
      Useful to process input from [stdin], or even large files where it would
      be impractical to parse the entire file before processing it. *)

end

