
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module Lexer = LexSmtlib
module Parser = ParseSmtlib

module type Term = Ast_smtlib.Term
module type Statement = Ast_smtlib.Statement

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) : sig

  val parse_file : string -> S.t list

end
