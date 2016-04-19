
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Term = Ast_zf.Term
module type Statement = Ast_zf.Statement

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) =
  Transformer.Make(L)(struct
    type token = Tokens_zf.token
    type statement = S.t
  end)(LexZf)(ParseZf.Make(L)(T)(S))

