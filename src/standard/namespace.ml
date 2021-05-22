
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Type definitions *)
(* ************************************************************************* *)

type value =
  | Integer
  | Rational
  | Real
  | Binary
  | Hexadecimal
  | Bitvector
  | String

type t =
  | Var
  | Sort
  | Term
  | Attr
  | Decl
  | Track
  | Value of value


(* Creation functions *)
(* ************************************************************************* *)

let var = Var
let sort = Sort
let term = Term
let attr = Attr
let decl = Decl
let track = Track


(* Std functions *)
(* ************************************************************************* *)

let value_discr = function
  | Integer -> 0
  | Rational -> 1
  | Real -> 2
  | Binary -> 3
  | Hexadecimal -> 4
  | Bitvector -> 5
  | String -> 6

let discr = function
  | Var -> 0
  | Sort -> 1
  | Term -> 2
  | Attr -> 3
  | Decl -> 4
  | Track -> 5
  | Value v -> 6 + value_discr v

let hash a = Hashtbl.hash a
let compare a b = compare (discr a) (discr b)
let equal a b = a == b || compare a b = 0

let print_value fmt = function
  | Integer -> Format.fprintf fmt "int"
  | Rational -> Format.fprintf fmt "rat"
  | Real -> Format.fprintf fmt "real"
  | Binary -> Format.fprintf fmt "bin"
  | Hexadecimal -> Format.fprintf fmt "hex"
  | Bitvector -> Format.fprintf fmt "bitv"
  | String -> Format.fprintf fmt "string"

let print fmt = function
  | Var -> Format.fprintf fmt "var"
  | Sort -> Format.fprintf fmt "sort"
  | Term -> Format.fprintf fmt "term"
  | Attr -> Format.fprintf fmt "attr"
  | Decl -> Format.fprintf fmt "decl"
  | Track -> Format.fprintf fmt "track"
  | Value value -> Format.fprintf fmt "value:%a" print_value value


(* Map *)
(* ************************************************************************* *)

module Map = Maps.Make(struct
    type nonrec t = t
    let compare = compare
  end)


