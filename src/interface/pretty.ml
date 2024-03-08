
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** Pretty printing annotations

    This module defines types to specify pretty printing annotations
    (such as associtativity, infix notations, etc...).
*)


(* Pretty types *)
(* ************************************************************************ *)

type name =
  | Exact of string
  | Renamed of string

type pos =
  | Infix
  | Prefix

type assoc =
  | Left
  | Right

type 'a print =
  | Ignore : _ print
  | P : (Format.formatter -> 'a -> unit) -> 'a print

