
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Interface for Dolmen languages modules *)

module type S = sig

  type token
  (** The type of tokens produced by the language lexer. *)

  type statement
  (** The type of top-level directives recognised by the parser. *)

  module Lexer : Lex.S
    with type token := token
  (** The Lexer module for the language. *)

  module Parser : Parse.S
    with type token := token
     and type statement := statement
  (** The Parser module for the language. *)

  val find : ?dir:string -> string -> string option
  (** Helper function to find a file using a language specification.
      Separates directory and file because most include directives in languages
      are relative to the directory of the original file being processed. *)

  val parse_file : string -> statement list
  (** Parse the given file.
      @param dir: optional directory to use if the file path is relative. *)

  val parse_input :
    [ `Stdin | `File of string ] -> (unit -> statement option) * (unit -> unit)
  (** Incremental parsing. Given an input to read (either a file, or stdin),
      returns a generator that will incrementally parse the statements,
      together with a cleanup function to close file descriptors.
      In case of a syntax error, the current line will be completely
      consumed and parsing will restart at the beginning of the next line.
      Useful to process input from [stdin], or even large files where it would
      be impractical to parse the entire file before processing it. *)

end

