
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Main Typedef *)
(* ************************************************************************* *)

type 'a t = ..
constraint 'a = < .. >
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
    | T of int
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
    | Not of int
    | And of int
    | Or of int
    | Nand of int
    | Nor of int
    | Xor of int
    | Xnor of int
    | Comp of int
    | Neg of int
    | Add of int
    | Sub of int
    | Mul of int
    | Udiv of int
    | Urem of int
    | Sdiv of int
    | Srem of int
    | Smod of int
    | Shl of int
    | Lshr of int
    | Ashr of int
    | Ult of int
    | Ule of int
    | Ugt of int
    | Uge of int
    | Slt of int
    | Sle of int
    | Sgt of int
    | Sge of int
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
    | T of int * int
    | RoundingMode
    | Fp of int * int
    | RoundNearestTiesToEven
    | RoundNearestTiesToAway
    | RoundTowardPositive
    | RoundTowardNegative
    | RoundTowardZero
    | Plus_infinity of int * int
    | Minus_infinity of int * int
    | Plus_zero of int * int
    | Minus_zero of int * int
    | NaN of int * int
    | Abs of int * int
    | Neg of int * int
    | Add of int * int
    | Sub of int * int
    | Mul of int * int
    | Div of int * int
    | Fma of int * int
    | Sqrt of int * int
    | Rem of int * int
    | RoundToIntegral  of int * int
    | Min of int * int
    | Max of int * int
    | Leq of int * int
    | Lt of int * int
    | Geq of int * int
    | Gt of int * int
    | Eq of int * int
    | IsNormal of int * int
    | IsSubnormal of int * int
    | IsZero of int * int
    | IsInfinite of int * int
    | IsNaN of int * int
    | IsNegative of int * int
    | IsPositive of int * int
    | Ieee_format_to_fp of int * int
    | To_fp of int * int * int * int
    | Of_real of int * int
    | Of_sbv of int * int * int
    | Of_ubv of int * int * int
    | To_ubv of int * int * int
    | To_sbv of int * int * int
    | To_real of int * int
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

