
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Id = Ast_ae.Id
module type Term = Ast_ae.Term
module type Statement = Ast_ae.Statement

module Make
    (L : Dolmen_intf.Location.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t) =
  Dolmen_std.Transformer.Make(L)(struct
    type token = Tokens_ae.token
    type statement = S.t
    let env = []
  end)(LexAe)(ParseAe.Make(L)(I)(T)(S))

