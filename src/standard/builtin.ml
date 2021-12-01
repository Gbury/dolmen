
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Main Typedef *)
(* ************************************************************************* *)

type 'a t = 'a Dolmen_intf.Builtin.t = .. constraint 'a = < .. >
(* Extensible variant type for builtin operations.
   Parameterized over the type of variables, constants, and terms. *)

(* Base builtins *)
(* ************************************************************************* *)

type _ t += Base

type _ t +=
  | Wildcard : { ty : 'ty option ref; } -> < ty : 'ty ; .. > t

type _ t += Kind | Type

type _ t += Unit | Univ

type _ t += Equal | Distinct

type _ t += Coercion

type _ t +=
  | Maps_to
  | Multi_trigger
  | Semantic_trigger

type _ t += Pi | Sigma


(* Boolean builtins *)
(* ************************************************************************* *)

module Prop = struct
  type _ t =
    | T (* Alias Prop *)
    | True | False
    | Neg | And | Or
    | Nand | Nor | Xor
    | Imply | Implied | Equiv
    | Ite
end

type 'a t += Prop of 'a Prop.t


(* Algebraic datatype builtins *)
(* ************************************************************************* *)

module Adt = struct
  type _ t =
    | Tester :
        { adt: 'ty_cst; case: int; cstr : 'term_cst; } ->
        < ty_cst : 'ty_cst ; term_cst : 'term_cst; .. > t
    | Constructor :
        { adt : 'ty_cst; case : int; } ->
        < ty_cst : 'ty_cst ; .. > t
    | Destructor :
        { adt : 'ty_cst; case : int; cstr : 'term_cst; field: int; } ->
        < ty_cst : 'ty_cst ; term_cst : 'term_cst; .. > t
end

type 'a t += Adt of 'a Adt.t


(* HO encoding into FO using Maps *)
(* ************************************************************************* *)

module Map = struct
  type _ t =
    | T
    | App
end

type 'a t += Map of 'a Map.t


(* Arithmetic builtins *)
(* ************************************************************************* *)

module Arith = struct
  type int_real = [ `Int | `Real ]
  type rat_real = [ `Rat | `Real ]
  type int_rat_real = [ `Int | `Rat | `Real ]
  type _ t =
    | Int | Integer of string
    | Rat | Rational of string
    | Real | Decimal of string
    | Lt of int_rat_real | Leq of int_rat_real
    | Gt of int_rat_real | Geq of int_rat_real
    | Minus of int_rat_real
    | Add of int_rat_real | Sub of int_rat_real
    | Mul of int_rat_real | Pow of int_real
    | Div of rat_real
    | Div_e of int_rat_real | Modulo_e of int_rat_real
    | Div_t of int_rat_real | Modulo_t of int_rat_real
    | Div_f of int_rat_real | Modulo_f of int_rat_real
    | Abs | Divisible
    | Is_int of int_rat_real | Is_rat of int_rat_real
    | Floor of int_rat_real | Floor_to_int of rat_real
    | Ceiling of int_rat_real | Truncate of int_rat_real | Round of int_rat_real
end

type 'a t += Arith of 'a Arith.t


(* Array builtins *)
(* ************************************************************************* *)

module Array = struct
  type _ t =
    | T | Const | Store | Select
end

type 'a t += Array of 'a Array.t

(* Bitvectors *)
(* ************************************************************************* *)

module Bitv = struct
  type _ t =
    | T of { n : int; }
    | Binary_lit of string
    | To_int of { n : int; signed : bool; }
    | Of_int of { n : int; }
    | Concat of { n : int; m : int }
    | Extract of { n : int; i : int; j : int }
    | Repeat of { n : int; k : int }
    | Zero_extend of { n : int; k : int }
    | Sign_extend of { n : int; k : int }
    | Rotate_right of { n : int; i : int }
    | Rotate_left of { n : int; i : int }
    | Not of { n : int; }
    | And of { n : int; }
    | Or of { n : int; }
    | Nand of { n : int; }
    | Nor of { n : int; }
    | Xor of { n : int; }
    | Xnor of { n : int; }
    | Comp of { n : int; }
    | Neg of { n : int; }
    | Add of { n : int; }
    | Sub of { n : int; }
    | Mul of { n : int; }
    | Udiv of { n : int; }
    | Urem of { n : int; }
    | Sdiv of { n : int; }
    | Srem of { n : int; }
    | Smod of { n : int; }
    | Shl of { n : int; }
    | Lshr of { n : int; }
    | Ashr of { n : int; }
    | Ult of { n : int; }
    | Ule of { n : int; }
    | Ugt of { n : int; }
    | Uge of { n : int; }
    | Slt of { n : int; }
    | Sle of { n : int; }
    | Sgt of { n : int; }
    | Sge of { n : int; }
    | Overflow_neg of { n : int; }
    | Overflow_add of { n : int; signed : bool; }
    | Overflow_sub of { n : int; signed : bool; }
    | Overflow_mul of { n : int; signed : bool; }
    | Overflow_div of { n : int; }
end

type 'a t += Bitv of 'a Bitv.t

(* Floats *)
(* ************************************************************************* *)

module Float = struct
  type _ t =
    | T of { e : int; s : int; }
    | RoundingMode
    | Fp of { e : int; s : int; }
    | RoundNearestTiesToEven
    | RoundNearestTiesToAway
    | RoundTowardPositive
    | RoundTowardNegative
    | RoundTowardZero
    | Plus_infinity of { e : int; s : int; }
    | Minus_infinity of { e : int; s : int; }
    | Plus_zero of { e : int; s : int; }
    | Minus_zero of { e : int; s : int; }
    | NaN of { e : int; s : int; }
    | Abs of { e : int; s : int; }
    | Neg of { e : int; s : int; }
    | Add of { e : int; s : int; }
    | Sub of { e : int; s : int; }
    | Mul of { e : int; s : int; }
    | Div of { e : int; s : int; }
    | Fma of { e : int; s : int; }
    | Sqrt of { e : int; s : int; }
    | Rem of { e : int; s : int; }
    | RoundToIntegral  of { e : int; s : int; }
    | Min of { e : int; s : int; }
    | Max of { e : int; s : int; }
    | Leq of { e : int; s : int; }
    | Lt of { e : int; s : int; }
    | Geq of { e : int; s : int; }
    | Gt of { e : int; s : int; }
    | Eq of { e : int; s : int; }
    | IsNormal of { e : int; s : int; }
    | IsSubnormal of { e : int; s : int; }
    | IsZero of { e : int; s : int; }
    | IsInfinite of { e : int; s : int; }
    | IsNaN of { e : int; s : int; }
    | IsNegative of { e : int; s : int; }
    | IsPositive of { e : int; s : int; }
    | Ieee_format_to_fp of { e : int; s : int; }
    | To_fp of { e1 : int; s1 : int; e2 : int; s2 : int; }
    | Of_real of { e : int; s : int; }
    | Of_sbv of { m : int; e : int; s : int; }
    | Of_ubv of { m : int; e : int; s : int; }
    | To_ubv of { m : int; e : int; s : int; }
    | To_sbv of { m : int; e : int; s : int; }
    | To_real of { e : int; s : int; }
end

type 'a t += Float of 'a Float.t


(* Strings *)
(* ************************************************************************* *)

module Str = struct

  type _ t =
    | T
    | Raw of string
    | Length
    | At
    | To_code
    | Of_code
    | Is_digit
    | To_int
    | Of_int
    | Concat
    | Sub
    | Index_of
    | Replace
    | Replace_all
    | Replace_re
    | Replace_re_all
    | Is_prefix
    | Is_suffix
    | Contains
    | Lexicographic_strict
    | Lexicographic_large
    | In_re

  (* String Regular languages *)
  module RegLan = struct
    type _ t =
      | T
      | Empty
      | All
      | Allchar
      | Of_string
      | Range
      | Concat
      | Union
      | Inter
      | Star
      | Cross
      | Complement
      | Diff
      | Option
      | Power of int
      | Loop of int * int
  end

end

type 'a t +=
  | Str of 'a Str.t
  | Regexp of 'a Str.RegLan.t

