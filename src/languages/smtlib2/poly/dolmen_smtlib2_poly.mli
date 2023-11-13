
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Smtlib language input *)

module type Id = Ast.Id
module type Term = Ast.Term
module type Statement = Ast.Statement
module type Extension = Ast.Extension
(** Implementation requirement for the Smtlib format. *)

module Print = Print
(** Printing functions. *)

module Make
    (L : Dolmen_intf.Location.S)
    (I : Id)
    (T : Term with type location := L.t and type id := I.t)
    (S : Statement with type location := L.t and type id := I.t and type term := T.t)
    (E : Extension with type location := L.t and type term := T.t and type statement := S.t) :
  Dolmen_intf.Language.S with type statement = S.t and type file := L.file
(** Functor to generate a parser for the Smtlib format. *)
