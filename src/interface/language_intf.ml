
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Generic type for language modules *)

module type S = sig

  type token

  type statement

  module Lexer : Lex_intf.S
    with type token := token

  module Parser : Parse_intf.S
    with type token := token
     and type statement := statement

  val parse_file : string -> statement list

end

