
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** TPTP language input *)

module type Id = Ast_tptp.Id
module type Term = Ast_tptp.Term
module type Statement = Ast_tptp.Statement
(** Implementation requirement for the TPTP format. *)

module Make
    (L : Dolmen_intf.Location.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t) :
  Dolmen_intf.Language.S with type statement = S.t
(** Functor to generate a parser for the TPTP format. *)

