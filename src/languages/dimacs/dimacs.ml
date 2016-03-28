
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Term = Ast_dimacs.Term
module type Statement = Ast_dimacs.Statement

module Lexer = LexDimacs
module Parser = ParseDimacs

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type atom := T.t) = struct

  module P = Parser.Make(L)(T)(S)

  let parse_file file =
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    ParseLocation.set_file lexbuf file;
    try
      P.file Lexer.token lexbuf
    with
    | P.Error ->
      let pos = L.of_lexbuf lexbuf in
      raise (L.Syntax_error (pos, ""))
    | LexDimacs.Error ->
      let pos = L.of_lexbuf lexbuf in
      raise (L.Lexing_error (pos, Lexing.lexeme lexbuf))
    | _ as e ->
      let pos = L.of_lexbuf lexbuf in
      raise (L.Uncaught (pos, e))

end

