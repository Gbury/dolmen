
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

(** Dimacs language input *)

module type Term = Ast_dimacs.Term
module type Statement = Ast_dimacs.Statement
(** Implementation requirement for the Dimacs format. *)

module Make
    (L : Dolmen_intf.Location.S)
    (T : Term with type location := L.t)
    (S : Statement with type location := L.t and type term := T.t) :
  Dolmen_intf.Language.S with type statement = S.t
(** Functor to generate a parser for the dimacs format. *)
