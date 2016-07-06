
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Id = Ast_dedukti.Id
module type Term = Ast_dedukti.Term
module type Statement = Ast_dedukti.Statement

module Make
    (L : ParseLocation.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t) =
  Transformer.Make(L)(struct
    type token = Tokens_dedukti.token
    type statement = S.t
    let env = []
  end)(LexDedukti)(ParseDedukti.Make(L)(I)(T)(S))
