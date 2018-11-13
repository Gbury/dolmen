
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Id = Ast_zf.Id
module type Term = Ast_zf.Term
module type Statement = Ast_zf.Statement

module Make
    (L : Dolmen_intf.Location.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t) =
  Dolmen_std.Transformer.Make(L)(struct
    type token = Tokens_zf.token
    type statement = S.t
    let env = []
  end)(LexZf)(ParseZf.Make(L)(I)(T)(S))

