
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Smtlib language input *)

module type Id = Ast_smtlib.Id
module type Term = Ast_smtlib.Term
module type Statement = Ast_smtlib.Statement
(** Implementation requirement for the Smtlib format. *)

module Make
    (L : Dolmen_intf.Location.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t) :
  Dolmen_intf.Language.S with type statement = S.t and type file := L.file
(** Functor to generate a parser for the Smtlib format. *)
