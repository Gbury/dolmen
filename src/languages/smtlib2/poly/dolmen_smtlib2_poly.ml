
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Id = Ast_smtlib.Id
module type Term = Ast_smtlib.Term
module type Statement = Ast_smtlib.Statement

module Make
    (L : Dolmen_intf.Location.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t) =
  Dolmen_std.Transformer.Make(L)(struct
    type token = Tokens_smtlib.token
    type statement = S.t
    let env = []
    let incremental = true
    let error s = Syntax_messages.message s
  end)(LexSmtlib)(ParseSmtlib.Make(L)(I)(T)(S))
