
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

type _ t += Kind | Type | Prop

type _ t += | Unit | Univ

type _ t += Coercion

type _ t +=
  | Maps_to
  | Multi_trigger
  | Semantic_trigger

(* Boolean builtins *)
(* ************************************************************************* *)

type _ t +=
  | True | False
  | Equal | Distinct
  | Neg | And | Or
  | Nand | Nor | Xor
  | Imply | Implied | Equiv

type _ t += Ite

type _ t += Pi | Sigma

(* Algebraic datatype builtins *)
(* ************************************************************************* *)

type _ t +=
  | Tester :
      { adt: 'ty_cst; case: int; cstr : 'term_cst; } ->
      < ty_cst : 'ty_cst ; term_cst : 'term_cst; .. > t
  | Constructor :
      { adt : 'ty_cst; case : int; } ->
      < ty_cst : 'ty_cst ; .. > t
  | Destructor :
      { adt : 'ty_cst; case : int; cstr : 'term_cst; field: int; } ->
      < ty_cst : 'ty_cst ; term_cst : 'term_cst; .. > t


(* Arithmetic builtins *)
(* ************************************************************************* *)

type rat_real = [ `Rat | `Real ]
type int_rat_real = [ `Int | `Rat | `Real ]

type _ t +=
  | Int | Integer of string
  | Rat | Rational of string
  | Real | Decimal of string
  | Lt of int_rat_real | Leq of int_rat_real
  | Gt of int_rat_real | Geq of int_rat_real
  | Minus of int_rat_real
  | Add of int_rat_real | Sub of int_rat_real
  | Mul of int_rat_real | Pow of int_rat_real
  | Div of rat_real
  | Div_e of int_rat_real | Modulo_e of int_rat_real
  | Div_t of int_rat_real | Modulo_t of int_rat_real
  | Div_f of int_rat_real | Modulo_f of int_rat_real
  | Abs | Divisible
  | Is_int of int_rat_real | Is_rat of int_rat_real
  | Floor of int_rat_real | Floor_to_int of rat_real
  | Ceiling of int_rat_real | Truncate of int_rat_real | Round of int_rat_real

(* arrays *)
type _ t +=
  | Array | Const | Store | Select

(* Bitvectors *)
type _ t +=
  | Bitv of int
  | Bitvec of string
  | Bitv_concat of { n : int; m : int }
  | Bitv_extract of { n : int; i : int; j : int }
  | Bitv_repeat of { n : int; k : int }
  | Bitv_zero_extend of { n : int; k : int }
  | Bitv_sign_extend of { n : int; k : int }
  | Bitv_rotate_right of { n : int; i : int }
  | Bitv_rotate_left of { n : int; i : int }
  | Bitv_not of int
  | Bitv_and of int
  | Bitv_or of int
  | Bitv_nand of int
  | Bitv_nor of int
  | Bitv_xor of int
  | Bitv_xnor of int
  | Bitv_comp of int
  | Bitv_neg of int
  | Bitv_add of int
  | Bitv_sub of int
  | Bitv_mul of int
  | Bitv_udiv of int
  | Bitv_urem of int
  | Bitv_sdiv of int
  | Bitv_srem of int
  | Bitv_smod of int
  | Bitv_shl of int
  | Bitv_lshr of int
  | Bitv_ashr of int
  | Bitv_ult of int
  | Bitv_ule of int
  | Bitv_ugt of int
  | Bitv_uge of int
  | Bitv_slt of int
  | Bitv_sle of int
  | Bitv_sgt of int
  | Bitv_sge of int

(* Floats *)
type _ t +=
  | Float of int * int
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
  | Fp_abs of int * int
  | Fp_neg of int * int
  | Fp_add of int * int
  | Fp_sub of int * int
  | Fp_mul of int * int
  | Fp_div of int * int
  | Fp_fma of int * int
  | Fp_sqrt of int * int
  | Fp_rem of int * int
  | Fp_roundToIntegral  of int * int
  | Fp_min of int * int
  | Fp_max of int * int
  | Fp_leq of int * int
  | Fp_lt of int * int
  | Fp_geq of int * int
  | Fp_gt of int * int
  | Fp_eq of int * int
  | Fp_isNormal of int * int
  | Fp_isSubnormal of int * int
  | Fp_isZero of int * int
  | Fp_isInfinite of int * int
  | Fp_isNaN of int * int
  | Fp_isNegative of int * int
  | Fp_isPositive of int * int
  | Ieee_format_to_fp of int * int
  | Fp_to_fp of int * int * int * int
  | Real_to_fp of int * int
  | Sbv_to_fp of int * int * int
  | Ubv_to_fp of int * int * int
  | To_ubv of int * int * int
  | To_sbv of int * int * int
  | To_real of int * int

(* Strings *)
type _ t +=
  | String
  | Str of string
  | Str_length
  | Str_at
  | Str_to_code
  | Str_of_code
  | Str_is_digit
  | Str_to_int
  | Str_of_int
  | Str_concat
  | Str_sub
  | Str_index_of
  | Str_replace
  | Str_replace_all
  | Str_replace_re
  | Str_replace_re_all
  | Str_is_prefix
  | Str_is_suffix
  | Str_contains
  | Str_lexicographic_strict
  | Str_lexicographic_large
  | Str_in_re

(* String Regular languages *)
type _ t +=
  | String_RegLan
  | Re_empty
  | Re_all
  | Re_allchar
  | Re_of_string
  | Re_range
  | Re_concat
  | Re_union
  | Re_inter
  | Re_star
  | Re_cross
  | Re_complement
  | Re_diff
  | Re_option
  | Re_power of int
  | Re_loop of int * int

