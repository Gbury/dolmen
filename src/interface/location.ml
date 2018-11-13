
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interface for file locations.
    This module defines an interface to store locations in files. These locations
    are used by the parsers to specify the locations of all parsed expressions
    whenever it is possible.
    This interface also requires some exceptions to be defined. These exceptions
    make use of locations to specify at which point in the file the parsing went wrong.
*)

module type S = sig

  type t
  (** The type of locations. *)

  exception Uncaught of t * exn
  (** The exception to be raised whenever an unexpected exception is raised during parsing. *)

  exception Lexing_error of t * string
  (** The exception to be raised when the lexer cannot parse the input. *)

  exception Syntax_error of t * string
  (** The exception to be raised whenever a syntax error is encountered by the parser. *)

  val of_lexbuf : Lexing.lexbuf -> t
  (** Make a position using a lexbuf directly. *)

  val mk_pos : Lexing.position -> Lexing.position -> t
  (** Make a position from two lewing positions. *)

end

