
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** TPTP language input *)

module type Term = Ast_tptp.Term
module type Statement = Ast_tptp.Statement
(** Implementation requirement for the TPTP format. *)

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) :
  Language_intf.S with type statement = S.t
(** Functor to generate a parser for the TPTP format. *)

