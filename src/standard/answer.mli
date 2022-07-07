
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard implementation of answers. *)


(** {2 Type definitions} *)

type term = Term.t
type location = Loc.t
type defs = Statement.defs
(** Type aliases for readability. *)

type descr =
  | Unsat
  | Sat of Statement.defs list option
  | Error of string

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


(** {2 Std function} *)

val print : Format.formatter -> t -> unit
(** Printing function *)


