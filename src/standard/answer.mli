
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard imlplementation of answers.
    This module provides a reasonable and standard implementation of statements,
    that can directly be used to instantiated the various functors of the dolmen library.
    These statements are closer to smtlib statements than to other languages statements
    because it is easier to express other languages statements using smtlib's than the
    other way around. Still, a generalisation of smtlib statements was needed so as not
    to lose some important distinctions between conjectures and assertions for instance.
*)


(** {2 Type definitions} *)

type term = Term.t
type location = Loc.t
type defs = Statement.defs
(** Type aliases for readability. *)

type descr =
  | Unsat
  | Sat of Statement.defs list option

and t = {
  id : Id.t option;
  descr : descr;
  attrs : term list;
  loc : location;
}

(** {2 implemented interfaces} *)

include Dolmen_intf.Stmt.Response
  with type t := t
   and type id := Id.t
   and type term := term
   and type location := location
   and type defs := defs


