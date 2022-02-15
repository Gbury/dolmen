
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definition *)
(* ************************************************************************* *)

module E = Dolmen.Std.Expr

type t =
  | Cst of { cst : E.Term.Const.t; }

let print fmt = function
  | Cst { cst } -> Format.fprintf fmt "%a" E.Term.Const.print cst

let compare t t' =
  match t, t' with
  | Cst { cst = c; }, Cst { cst = c'; } -> E.Term.Const.compare c c'

let ops = Value.ops ~compare ~print


(* Type definition *)
(* ************************************************************************* *)

let from_cst cst =
  Value.mk ~ops (Cst { cst; })


