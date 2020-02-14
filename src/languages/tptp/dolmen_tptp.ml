
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Id = Ast_tptp.Id
module type Term = Ast_tptp.Term
module type Statement = Ast_tptp.Statement

module Make
    (L : Dolmen_intf.Location.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t) =
  Dolmen_std.Transformer.Make(L)(struct
    type token = Tokens_tptp.token
    type statement = S.t
    let env = ["TPTP"]
    let incremental = true
  end)(LexTptp)(ParseTptp.Make(L)(I)(T)(S))

