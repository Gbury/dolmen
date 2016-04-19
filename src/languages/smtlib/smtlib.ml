
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Term = Ast_smtlib.Term
module type Statement = Ast_smtlib.Statement

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) = struct

  module P = ParseSmtlib.Make(L)(T)(S)

  module M = Transformer.Make(L)(struct
      type token = Tokens_smtlib.token
      type statement = S.t
    end)(LexSmtlib)(P)

  include M

end
