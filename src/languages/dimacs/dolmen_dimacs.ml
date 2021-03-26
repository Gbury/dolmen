
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Term = Ast.Term
module type Statement = Ast.Statement

module Make
    (L : Dolmen_intf.Location.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) =
  Dolmen_std.Transformer.Make(L)(struct
    type token = Tokens.token
    type statement = S.t
    let env = []
    let incremental = true
    let error s = Syntax_messages.message s
  end)(Lexer)(Parser.Make(L)(T)(S))

