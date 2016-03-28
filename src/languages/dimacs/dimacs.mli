
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module Lexer = LexDimacs
module Parser = ParseDimacs

module type Term = Ast_dimacs.Term
module type Statement = Ast_dimacs.Statement

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type atom := T.t) : sig

  val parse_file : string -> S.t list

end
