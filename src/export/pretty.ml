
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type pos =
  | Infix
  | Prefix (**)
(** The type for printing positions of identifiers. *)

type assoc =
  | Left
  | Right (**)
(** Associativity of operators. *)

type t = {
  name  : string;
  pos   : pos;
  assoc : assoc option;
}

let mk ?assoc name pos =
  { name; pos; assoc; }

