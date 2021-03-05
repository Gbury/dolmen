
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

  type file
  (** A store for various meta-data about an input file,
      can be used to optimize representation of locations. *)

  exception Uncaught of t * exn * Printexc.raw_backtrace
  (** The exception to be raised whenever an unexpected exception is raised during parsing. *)

  exception Lexing_error of t * string
  (** The exception to be raised when the lexer cannot parse the input. *)

  exception Syntax_error of t * [
      | `Regular of Msg.t
      | `Advanced of Msg.t * Msg.t * Msg.t
    ]
  (** [Syntax_error (loc, msg)] denotes a syntax error at the given location.
      In the [`Advanced (prod, parsed, expected)] case,
      - prod is a delayed message to print in order to identify which
        production/syntax construction the parser was trying to reduce,
      - parsed is a description of the token which raised the error,
      - expected is a messages describing what would have been corect
        tokens/inputs at that point. *)

  val of_lexbuf : Lexing.lexbuf -> t
  (** Make a position using a lexbuf directly. *)

  val mk_pos : Lexing.position -> Lexing.position -> t
  (** Make a position from two lewing positions. *)

  val mk_file : string -> file
  (** Create meta-data for a given filename. *)

  val newline : file -> Lexing.lexbuf -> unit
  (** Offer a way for the file meta-data to store the current location
      of the lexbuf as the start of a new line. *)

end

