
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Zipperposition format input *)

module type Term = Ast_zf.Term
module type Statement = Ast_zf.Statement
(** Implementation requirements for the Zipperposition format. *)

module Make
    (L : ParseLocation.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) :
  Language_intf.S with type statement = S.t
(** Functor to generate a parser for the Zipperposition format. *)

