
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module type Term = Ast_smtlib.Term
module type Statement = Ast_smtlib.Statement
(** Implementation requirement for the Smtlib format. *)

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) :
  Language_intf.S with type statement = S.t
(** Functor to generate a parser for the Smtlib format. *)
