
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Term = Ast_dimacs.Term
module type Statement = Ast_dimacs.Statement

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type atom := T.t) = struct

  module P = ParseDimacs.Make(L)(T)(S)

  module M = Transformer.Make(L)(struct
      type token = Tokens_dimacs.token
      type statement = S.t
    end)(LexDimacs)(P)

  include M

end

