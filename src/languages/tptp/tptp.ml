
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Term = Ast_tptp.Term
module type Statement = Ast_tptp.Statement

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) =
  Transformer.Make(L)(struct
    type token = Tokens_tptp.token
    type statement = S.t
    let env = ["TPTP"]
  end)(LexTptp)(ParseTptp.Make(L)(T)(S))
