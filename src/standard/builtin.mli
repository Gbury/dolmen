
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** This module defines the builtins that are defined by Dolmen.

    Builtins are particularly used in typed expressions see {!Dolmen.Std.Expr},
    in order to give more information about constants which have builtin
    semantics.

    Users are encouraged to match builtins rather than specific symbols when
    inspecting typed expressions, as this basically allows to match on the
    semantics of an identifier rather than matching on the syntaxic value of an
    identifier. For instance, equality can take an arbitrary number of
    arguments, and thus in order to have well-typed terms, each arity of
    equality gives rise to a different symbol (because the symbol's type
    depends on the arity desired), but all these symbols have the [Equal]
    builtin.

    In the following we will use pseudo-code to describe the arity and actual
    type associated to builtins. These will follow ocaml's notation for types
    with an additional syntax using dots for arbitrary arity.  Some examples:
    - [ttype] is a type constant
    - [ttype -> ttype] is a type constructor (e.g. [list])
    - [int] is a constant of type [int]
    - [float -> int] is a unary function
    - ['a. 'a -> 'a] is a polymorphic unary function
    - ['a. 'a -> ... -> Prop] describes a family of functions that take
      a type and then an arbitrary number of arguments of that type, and
      return a proposition (this is for instance the type of equality).

    Additionally, due to some languages having overloaded operators, and in
    order to not have too verbose names, some of these builtins may have
    overloaded signtures, such as comparisons on numbers which can operate on
    integers, rationals, or reals. Note that arbitrary arity operators (well
    family of operators) can be also be seen as overloaded operators.
    Overloaded types (particularly for numbers) are written:
    - [{a=(Int|Rational|Real)} a -> a -> Prop], where the notable difference
      with polymorphic functions is that functions of this type does not
      take a type argument.
*)


(** {2 Type definition} *)
(*  ************************************************************************* *)

type _ t = ..
(* Extensible variant type for builtin operations.
   Parameterized over an object type that will record the type used for
   various constants and/or terms. *)


(** {2 Base Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | Base
  (** The base builtin; it is the default builtin for identifiers. *)

type _ t +=
  | Wildcard : { ty : 'ty option ref; } -> < ty : 'ty ; .. > t
  (** Wildcards, currently used internally to represent implicit type
      variables during type-checking. *)

type _ t +=
  | Kind
  (** Used for the type of [Type].
      It is an error to try and access the type of kind. *)
  | Type
  (** Builtin used to represent the type of types. *)
  | Prop
  (** [Prop: ttype]: the builtin type constant for the type of
      propositions / booleans. *)

type _ t +=
  | Unit
  (** The unit type, which has only one element (named void). *)
  | Univ
  (** [Univ: ttype]: a builtin type constant used for languages
      with a default type for elements (such as tptp's `$i`). *)

type _ t +=
  | Coercion
  (** [Coercion: 'a 'b. 'a -> 'b]:
      Coercion/cast operator, i.e. allows to cast values of some type to
      another type. This is a polymorphic operator that takes two type
      arguments [a] and [b], a value of type [a], and returns a value of
      type [b].
      The interpretation/semantics of this cast can remain
      up to the user. This operator is currently mainly used to cast
      numeric types when this transormation is exact (i.e. an integer
      casted into a rational, which is always possible and exact,
      or the cast of a rational into an integer, as long as the cast is
      guarded by a clause verifying the rational is an integer). *)


(** {2 Boolean Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | True      (** [True: Prop]: the [true] proposition. *)
  | False     (** [False: Prop]: the [false] proposition. *)
  | Equal     (** [Equal: 'a. 'a -> ... -> Prop]: equality beetween values. *)
  | Distinct  (** [Distinct: 'a. 'a -> ... -> Prop]: pairwise dis-equality beetween arguments. *)
  | Neg       (** [Neg: Prop -> Prop]: propositional negation. *)
  | And       (** [And: Prop -> Prop]: propositional conjunction. *)
  | Or        (** [Or: Prop -> ... -> Prop]: propositional disjunction. *)
  | Nand      (** [Nand: Prop -> Prop -> Prop]: propositional negated conjunction. *)
  | Nor       (** [Nor: Prop -> Prop -> Prop]: propositional negated disjunction. *)
  | Xor       (** [Xor: Prop -> Prop -> Prop]: ppropositional exclusive disjunction. *)
  | Imply     (** [Imply: Prop -> Prop -> Prop]: propositional implication. *)
  | Implied   (** [Implied: Prop -> Prop -> Prop]: reverse propositional implication. *)
  | Equiv     (** [Equiv: Prop -> Prop -> Prop]: propositional Equivalence. *)

type _ t +=
  | Ite
  (** [Ite: 'a. Prop -> 'a -> 'a -> 'a]: branching operator. *)

type _ t +=
  | Pi
  (** [Pi: 'a. ('a -> Prop) -> Prop]: higher-order encoding of universal quantification. *)
  | Sigma
  (** [Sigma: 'a. ('a -> Prop) -> Prop]: higher-order encoding of existencial quantification. *)


(** {2 Algebraic datatype Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | Tester :
      { cstr : 'term_cst; } ->
      < term_cst : 'term_cst ; .. > t
  (** [Tester { cstr; }] is the tester for constructor [cstr]. *)
  | Constructor :
      { adt : 'ty_cst; case : int; } ->
      < ty_cst : 'ty_cst ; .. > t
  (** [Constructor { adt; case}] is the case-th constructor of the algebraic
      datatype defined by [adt]. *)
  | Destructor :
      { adt : 'ty_cst; cstr : 'term_cst; case : int; field: int; } ->
      < ty_cst : 'ty_cst ; term_cst : 'term_cst; .. > t
  (** [Destructor { adt; cstr; case; field; }] is the destructor retuning the
      field-th argument of the case-th constructor of type [adt] which should
      be [cstr]. *)


(** {2 Arithmetic Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | Int
  (** [Int: ttype] the type for signed integers of arbitrary precision. *)
  | Integer of string
  (** [Integer s: Int]: integer litteral. The string [s] should be the
      decimal representation of an integer with arbitrary precision (hence
      the use of strings rather than the limited precision [int]). *)
  | Rat
  (** [Rat: ttype] the type for signed rationals. *)
  | Rational of string
  (** [Rational s: Rational]: rational litteral. The string [s] should be
      the decimal representation of a rational (see the various languages
      spec for more information). *)
  | Real
  (** [Real: ttype] the type for signed reals. *)
  | Decimal of string
  (** [Decimal s: Real]: real litterals. The string [s] should be a
      floating point representatoin of a real. Not however that reals
      here means the mathematical abstract notion of real numbers, including
      irrational, non-algebric numbers, and is thus not restricted to
      floating point numbers, although these are the only litterals
      supported. *)
  | Lt
  (** [Lt: {a=(Int|Rational|Real)} a -> a -> Prop]:
      strict comparison (less than) on numbers
      (whether integers, rationals, or reals). *)
  | Leq
  (** [Leq:{a=(Int|Rational|Real)} a -> a -> Prop]:
      large comparison (less or equal than) on numbers
      (whether integers, rationals, or reals). *)
  | Gt
  (** [Gt:{a=(Int|Rational|Real)} a -> a -> Prop]:
      strict comparison (greater than) on numbers
      (whether integers, rationals, or reals). *)
  | Geq
  (** [Geq:{a=(Int|Rational|Real)} a -> a -> Prop]:
      large comparison (greater or equal than) on numbers
      (whether integers, rationals, or reals). *)
  | Minus
  (** [Minus:{a=(Int|Rational|Real)} a -> a]:
      arithmetic unary negation/minus on numbers
      (whether integers, rationals, or reals). *)
  | Add
  (** [Add:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic addition on numbers
      (whether integers, rationals, or reals). *)
  | Sub
  (** [Sub:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic substraction on numbers
      (whether integers, rationals, or reals). *)
  | Mul
  (** [Mul:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic multiplication on numbers
      (whether integers, rationals, or reals). *)
  | Div
  (** [Div:{a=(Rational|Real)} a -> a -> a]:
      arithmetic exact division on numbers
      (rationals, or reals, but **not** integers). *)
  | Div_e
  (** [Div_e:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic integer euclidian quotient
      (whether integers, rationals, or reals).
      If D is positive then [Div_e (N,D)] is the floor
      (in the type of N and D) of the real division [N/D],
      and if D is negative then [Div_e (N,D)] is the ceiling
      of [N/D]. *)
  | Div_t
  (** [Div_t:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic integer truncated quotient
      (whether integers, rationals, or reals).
      [Div_t (N,D)] is the truncation of the real
      division [N/D]. *)
  | Div_f
  (** [Div_f:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic integer floor quotient
      (whether integers, rationals, or reals).
      [Div_t (N,D)] is the floor of the real
      division [N/D]. *)
  | Modulo_e
  (** [Modulo_e:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic integer euclidian remainder
      (whether integers, rationals, or reals).
      It is defined by the following equation:
      [Div_e (N, D) * D + Modulo(N, D) = N]. *)
  | Modulo_t
  (** [Modulo_t:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic integer truncated remainder
      (whether integers, rationals, or reals).
      It is defined by the following equation:
      [Div_t (N, D) * D + Modulo_t(N, D) = N]. *)
  | Modulo_f
  (** [Modulo_f:{a=(Int|Rational|Real)} a -> a -> a]:
      arithmetic integer floor remainder
      (whether integers, rationals, or reals).
      It is defined by the following equation:
      [Div_f (N, D) * D + Modulo_f(N, D) = N]. *)
  | Abs
  (** [Abs: Int -> Int]:
      absolute value on integers. *)
  | Divisible
  (** [Divisible: Int -> Int -> Prop]:
      divisibility predicate on integers. Smtlib restricts
      applications of this predicate to have a litteral integer
      for the divisor/second argument. *)
  | Is_int
  (** [Is_int:{a=(Int|Rational|Real)} a -> Prop]:
      integer predicate for numbers: is the given number
      an integer. *)
  | Is_rat
  (** [Is_rat:{a=(Int|Rational|Real)} a -> Prop]:
      rational predicate for numbers: is the given number
      an rational. *)
  | Floor
  (** [Floor:{a=(Int|Rational|Real)} a -> a]:
      floor function on numbers, defined in tptp as
      the largest integer not greater than the argument. *)
  | Ceiling
  (** [Ceiling:{a=(Int|Rational|Real)} a -> a]:
      ceiling function on numbers, defined in tptp as
      the smallest integer not less than the argument. *)
  | Truncate
  (** [Truncate:{a=(Int|Rational|Real)} a -> a]:
      ceiling function on numbers, defined in tptp as
      the nearest integer value with magnitude not greater
      than the absolute value of the argument. *)
  | Round
  (** [Round:{a=(Int|Rational|Real)} a -> a]:
      rounding function on numbers, defined in tptp as
      the nearest intger to the argument; when the argument
      is halfway between two integers, the nearest even integer
      to the argument. *)


(** {2 Arrays Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | Array
  (** [Array: ttype -> ttype -> ttype]: the type constructor for
      polymorphic functional arrays. An [(src, dst) Array] is an array
      from expressions of type [src] to expressions of type [dst].
      Typically, such arrays are immutables. *)
  | Store
  (** [Store: 'a 'b. ('a, 'b) Array -> 'a -> 'b -> ('a, 'b) Array]:
      store operation on arrays. Returns a new array with the key bound
      to the given value (shadowing the previous value associated to
      the key). *)
  | Select
  (** [Select: 'a 'b. ('a, 'b) Array -> 'a -> 'b]:
      select operation on arrays. Returns the value associated to the
      given key. Typically, functional arrays are complete, i.e. all
      keys are mapped to a value. *)


(** {2 Bitvectors Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | Bitv of int
  (** [Bitv n: ttype]: type constructor for bitvectors of length [n]. *)
  | Bitvec of string
  (** [Bitvec s: Bitv]: bitvector litteral. The string [s] should
      be a binary representation of bitvectors using characters
      ['0'], and ['1'] (lsb last) *)
  | Bitv_concat
  (** [Bitv_concat: Bitv(n) -> Bitv(m) -> Bitv(n+m)]:
      concatenation operator on bitvectors. *)
  | Bitv_extract of int * int
  (** [Bitv_extract(i, j): Bitv(n) -> Bitv(i - j + 1)]:
      bitvector extraction, from index [j] up to [i] (both included). *)
  | Bitv_repeat
  (** [Bitv_repeat: Bitv(n) -> Bitv(n*k)]:
      bitvector repeatition. NOTE: inlcude [k] in the builtin ? *)
  | Bitv_zero_extend
  (** [Bitv_zero_extend: Bitv(n) -> Bitv(n + k)]:
      zero extension for bitvectors (produces a representation of the
      same unsigned integer). *)
  | Bitv_sign_extend
  (** [Bitv_sign_extend: Bitv(n) -> Bitv(n + k)]:
      sign extension for bitvectors ((produces a representation of the
      same signed integer). *)
  | Bitv_rotate_right of int
  (** [Bitv_rotate_right(i): Bitv(n) -> Bitv(n)]:
      logical rotate right for bitvectors by [i]. *)
  | Bitv_rotate_left of int
  (** [Bitv_rotate_left(i): Bitv(n) -> Bitv(n)]:
      logical rotate left for bitvectors by [i]. *)
  | Bitv_not
  (** [Bitv_not: Bitv(n) -> Bitv(n)]:
      bitwise negation for bitvectors. *)
  | Bitv_and
  (** [Bitv_and: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      bitwise conjunction for bitvectors. *)
  | Bitv_or
  (** [bitv_or: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      bitwise disjunction for bitvectors. *)
  | Bitv_nand
  (** [Bitv_nand: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      bitwise negated conjunction for bitvectors.
      [Bitv_nand s t] abbreviates [Bitv_not (Bitv_and s t))]. *)
  | Bitv_nor
  (** [Bitv_nor: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      bitwise negated disjunction for bitvectors.
      [Bitv_nor s t] abbreviates [Bitv_not (Bitv_or s t))]. *)
  | Bitv_xor
  (** [Bitv_xor: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      bitwise exclusive disjunction for bitvectors.
      [Bitv_xor s t] abbreviates
      [Bitv_or (Bitv_and s (Bitv_not t))
               (Bitv_and (Bitv_not s) t) ]. *)
  | Bitv_xnor
  (** [Bitv_xnor: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      bitwise negated exclusive disjunction for bitvectors.
      [Bitv_xnor s t] abbreviates
      [Bitv_or (Bitv_and s t)
               (Bitv_and (Bitv_not s) (Bitv_not t))]. *)
  | Bitv_comp
  (** [Bitv_comp: Bitv(n) -> Bitv(n) -> Bitv(1)]:
      Returns the constant bitvector ["1"] is all bits are equal,
      and the bitvector ["0"] if not. *)
  | Bitv_neg
  (** [Bitv_neg: Bitv(n) -> Bitv(n)]:
      2's complement unary minus. *)
  | Bitv_add
  (** [Bitv_add: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      addition modulo 2^n. *)
  | Bitv_sub
  (** [Bitv_sub: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      2's complement subtraction modulo 2^n. *)
  | Bitv_mul
  (** [Bitv_mul: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      multiplication modulo 2^n. *)
  | Bitv_udiv
  (** [Bitv_udiv: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      unsigned division, truncating towards 0. *)
  | Bitv_urem
  (** [Bitv_urem: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      unsigned remainder from truncating division. *)
  | Bitv_sdiv
  (** [Bitv_sdiv: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      2's complement signed division. *)
  | Bitv_srem
  (** [Bitv_srem: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      2's complement signed remainder (sign follows dividend). *)
  | Bitv_smod
  (** [Bitv_smod: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      2's complement signed remainder (sign follows divisor). *)
  | Bitv_shl
  (** [Bitv_shl: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      shift left (equivalent to multiplication by 2^x where x
      is the value of the second argument). *)
  | Bitv_lshr
  (** [Bitv_lshr: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      logical shift right (equivalent to unsigned division by 2^x,
      where x is the value of the second argument). *)
  | Bitv_ashr
  (** [Bitv_ashr: Bitv(n) -> Bitv(n) -> Bitv(n)]:
      Arithmetic shift right, like logical shift right except that
      the most significant bits of the result always copy the most
      significant bit of the first argument. *)
  | Bitv_ult
  (** [Bitv_ult: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for unsigned less-than. *)
  | Bitv_ule
  (** [Bitv_ule: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for unsigned less than or equal. *)
  | Bitv_ugt
  (** [Bitv_ugt: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for unsigned greater-than. *)
  | Bitv_uge
  (** [Bitv_uge: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for unsigned greater than or equal. *)
  | Bitv_slt
  (** [Bitv_slt: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for signed less-than. *)
  | Bitv_sle
  (** [Bitv_sle: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for signed less than or equal. *)
  | Bitv_sgt
  (** [Bitv_sgt: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for signed greater-than. *)
  | Bitv_sge
  (** [Bitv_sge: Bitv(n) -> Bitv(n) -> Prop]:
      binary predicate for signed greater than or equal. *)


(** {2 Floats Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | RoundingMode
  (** [RoundingMode: ttype]: type for enumerated type of rounding modes. *)
  | RoundNearestTiesToEven
  (** [RoundNearestTiesToEven : RoundingMode]: *)
  | RoundNearestTiesToAway
  (** [RoundNearestTiesToAway : RoundingMode]: *)
  | RoundTowardPositive
  (** [RoundTowardPositive : RoundingMode *)
  | RoundTowardNegative
  (** [RoundTowardNegative : RoundingMode *)
  | RoundTowardZero
  (** [RoundTowardZero : RoundingMode *)
  | Float of int * int
  (** [Float(e,s): ttype]: type constructor for floating point of exponent of
     size [e] and significand of size [s] (hidden bit included). Those size are
     greater than 1 *)
  | Fp of int * int
  (** [Fp(e, s): Bitv(1) -> Bitv(e) -> Bitv(s-1) -> Fp(e,s)]: bitvector literal.
      The IEEE-format is used for the conversion [sb^se^ss].
      All the NaN are converted to the same value. *)
  | Plus_infinity of int * int
  (** [Plus_infinity(s,e) : Fp(s,e)] *)
  | Minus_infinity of int * int
  (** [Minus_infinity(s,e) : Fp(s,e)] *)
  | Plus_zero of int * int
  (** [Plus_zero(s,e) : Fp(s,e)] *)
  | Minus_zero of int * int
  (** [Minus_zero(s,e) : Fp(s,e)] *)
  | NaN of int * int
  (** [NaN(s,e) : Fp(s,e)] *)
  | Fp_abs  of int * int
  (** [Fp_abs(s,e): Fp(s,e) -> Fp(s,e)]: absolute value *)
  | Fp_neg  of int * int
  (** [Fp_neg(s,e): Fp(s,e) -> Fp(s,e)]: negation *)
  | Fp_add  of int * int
  (** [Fp_add(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: addition *)
  | Fp_sub  of int * int
  (** [Fp_sub(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: subtraction *)
  | Fp_mul  of int * int
  (** [Fp_mul(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: multiplication *)
  | Fp_div  of int * int
  (** [Fp_div(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: division *)
  | Fp_fma  of int * int
  (** [Fp_fma(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e)]: fuse multiply add *)
  | Fp_sqrt  of int * int
  (** [Fp_sqrt(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e)]: square root *)
  | Fp_rem  of int * int
  (** [Fp_rem(s,e): Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: remainder *)
  | Fp_roundToIntegral  of int * int
  (** [Fp_roundToIntegral(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e)]: round to integral *)
  | Fp_min  of int * int
  (** [Fp_min(s,e): Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: minimum *)
  | Fp_max  of int * int
  (** [Fp_max(s,e): Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: maximum *)
  | Fp_leq  of int * int
  (** [Fp_leq(s,e): Fp(s,e) -> Fp(s,e) -> Prop]: IEEE less or equal *)
  | Fp_lt  of int * int
  (** [Fp_lt(s,e): Fp(s,e) -> Fp(s,e) -> Prop]: IEEE less than *)
  | Fp_geq  of int * int
  (** [Fp_geq(s,e): Fp(s,e) -> Fp(s,e) -> Prop]: IEEE greater or equal *)
  | Fp_gt  of int * int
  (** [Fp_gt(s,e): Fp(s,e) -> Fp(s,e) -> Prop]: IEEE greater than *)
  | Fp_eq  of int * int
  (** [Fp_eq(s,e): Fp(s,e) -> Fp(s,e) -> Prop]: IEEE equality *)
  | Fp_isNormal  of int * int
  (** [Fp_isNormal(s,e): Fp(s,e) -> Prop]: test if it is a normal floating point *)
  | Fp_isSubnormal  of int * int
  (** [Fp_isSubnormal(s,e): Fp(s,e) -> Prop]: test if it is a subnormal floating point *)
  | Fp_isZero  of int * int
  (** [Fp_isZero(s,e): Fp(s,e) -> Prop]: test if it is a zero *)
  | Fp_isInfinite  of int * int
  (** [Fp_isInfinite(s,e): Fp(s,e) -> Prop]: test if it is an infinite *)
  | Fp_isNaN  of int * int
  (** [Fp_isNaN(s,e): Fp(s,e) -> Prop]: test if it is Not a Number *)
  | Fp_isNegative  of int * int
  (** [Fp_isNegative(s,e): Fp(s,e) -> Prop]: test if it is negative *)
  | Fp_isPositive  of int * int
  (** [Fp_isPositive(s,e): Fp(s,e) -> Prop]: test if it is positive *)
  | Ieee_format_to_fp of int * int
  (** [Ieee_format_to_fp(s,e): Bv(s+e) -> Fp(s,e)]: Convert from IEEE interchange format *)
  | Fp_to_fp of int * int * int * int
  (** [Fp_to_fp(s1,e1,s2,e2): RoundingMode -> Fp(s1,e1) -> Fp(s2,e2)]: Convert from another floating point format *)
  | Real_to_fp of int * int
  (** [Real_to_fp(s,e): RoundingMode -> Real -> Fp(s,e)]: Convert from a real *)
  | Sbv_to_fp of int * int * int
  (** [Sbv_to_fp(m,s,e): RoundingMode -> Bitv(m) -> Fp(s,e)]: Convert from a signed integer *)
  | Ubv_to_fp of int * int * int
  (** [Ubv_to_fp(m,s,e): RoundingMode -> Bitv(m) -> Fp(s,e)]: Convert from a unsigned integer *)
  | To_ubv of int * int * int
  (** [To_ubv(s,e,m): RoundingMode -> Fp(s,e) -> Bitv(m)]: Convert to an unsigned integer *)
  | To_sbv of int * int * int
  (** [To_ubv(s,e,m): RoundingMode -> Fp(s,e) -> Bitv(m)]: Convert to an signed integer *)
  | To_real of int * int
  (** [To_real(s,e,m): RoundingMode -> Fp(s,e) -> Real]: Convert to real *)


(** {2 String and Regexp Builtins} *)
(*  ************************************************************************* *)

type _ t +=
  | String
  (** [String: ttype]: type constructor for strings. *)
  | Str of string
  (** [Str s: String]: string literals. *)
  | Str_length
  (** [Str_length: String -> Int]: string length. *)
  | Str_at
  (** [Str_at: String -> Int -> String]:
      Singleton string containing a character at given position
      or empty string when position is out of range.
      The leftmost position is 0. *)
  | Str_to_code
  (** [Str_to_code: String -> Int]:
      [Str_to_code s] is the code point of the only character of s,
      if s is a singleton string; otherwise, it is -1. *)
  | Str_of_code
  (** [Str_of_code: Int -> String]:
      [Str_of_code n] is the singleton string whose only character is
      code point n if n is in the range [0, 196607]; otherwise, it is the
      empty string. *)
  | Str_is_digit
  (** [Str_is_digit: String -> Prop]: Digit check
      [Str.is_digit s] is true iff s consists of a single character which is
      a decimal digit, that is, a code point in the range 0x0030 ... 0x0039. *)
  | Str_to_int
  (** [Str_to_int: String -> Int]: Conversion to integers
      [Str.to_int s] with s consisting of digits (in the sense of str.is_digit)
      evaluates to the positive integer denoted by s when seen as a number in
      base 10 (possibly with leading zeros).
      It evaluates to -1 if s is empty or contains non-digits. *)
  | Str_of_int
  (** [Str_of_int : Int -> String]: Conversion from integers.
      [Str.from_int n] with n non-negative is the corresponding string in
      decimal notation, with no leading zeros. If n < 0, it is the empty string. *)
  | Str_concat
  (** [Str_concat: String -> String -> String]: string concatenation. *)
  | Str_sub
  (** [Str_sub: String -> Int -> Int -> String]:
      [Str_sub s i n] evaluates to the longest (unscattered) substring
      of s of length at most n starting at position i.
      It evaluates to the empty string if n is negative or i is not in
      the interval [0,l-1] where l is the length of s. *)
  | Str_index_of
  (** [Str_index_of: String -> String -> Int -> Int]:
      Index of first occurrence of second string in first one starting at
      the position specified by the third argument.
      [Str_index_of s t i], with 0 <= i <= |s| is the position of the first
      occurrence of t in s at or after position i, if any.
      Otherwise, it is -1. Note that the result is i whenever i is within
      the range [0, |s|] and t is empty. *)
  | Str_replace
  (** [Str_replace: String -> String -> String -> String]: Replace
      [Str_replace s t t'] is the string obtained by replacing the first
      occurrence of t in s, if any, by t'. Note that if t is empty, the
      result is to prepend t' to s; also, if t does not occur in s then
      the result is s. *)
  | Str_replace_all
  (** [Str_replace_all: String -> String -> String -> String]:
      [Str_replace_all s t t’] is s if t is the empty string. Otherwise, it
      is the string obtained from s by replacing all occurrences of t in s
      by t’, starting with the first occurrence and proceeding in
      left-to-right order. *)
  | Str_replace_re
  (** [Str_replace_re: String -> String_RegLan -> String -> String]:
      [Str_replace_re s r t] is the string obtained by replacing the
      shortest leftmost non-empty match of r in s, if any, by t.
      Note that if t is empty, the result is to prepend t to s. *)
  | Str_replace_re_all
  (** [Str_replace_re_all: String -> String_RegLan -> String -> String]:
      [Str_replace_re_all s r t] is the string obtained by replacing,
      left-to right, each shortest *non-empty* match of r in s by t. *)
  | Str_is_prefix
  (** [Str_is_prefix: String -> String -> Prop]: Prefix check
      [Str_is_prefix s t] is true iff s is a prefix of t. *)
  | Str_is_suffix
  (** [Str_is_suffix: String -> String -> Prop]: Suffix check
      [Str_is_suffix s t] is true iff s is a suffix of t. *)
  | Str_contains
  (** [Str_contains: String -> String -> Prop]: Inclusion check
      [Str_contains s t] is true iff s contains t. *)
  | Str_lexicographic_strict
  (** [Str_lexicographic_strict: String -> String -> Prop]:
      lexicographic ordering (strict). *)
  | Str_lexicographic_large
  (** [Str_lexicographic_large: String -> String -> Prop]:
      reflexive closure of the lexicographic ordering. *)
  | Str_in_re
  (** [Str_in_re: String -> String_RegLan -> Prop]: set membership. *)

(* String Regular languages *)
type _ t +=
  | String_RegLan
  (** [String_RegLan: ttype]:
      type constructor for Regular languages over strings. *)
  | Re_empty
  (** [Re_empty: String_RegLan]:
      the empty language. *)
  | Re_all
  (** [Re_all: String_RegLan]:
      the language of all strings. *)
  | Re_allchar
  (** [Re_allchar: String_RegLan]:
      the language of all singleton strings. *)
  | Re_of_string
  (** [Re_of_string: String -> String_RegLan]:
      the singleton language with a single string. *)
  | Re_range
  (** [Re_range: String -> String -> String_RegLan]: Language range
      [Re_range s1 s2] is the set of all *singleton* strings [s] such that
      [Str_lexicographic_large s1 s s2] provided [s1] and [s1] are singleton.
      Otherwise, it is the empty language. *)
  | Re_concat
  (** [Re_concat: String_RegLan -> String_RegLan -> String_RegLan]:
      language concatenation. *)
  | Re_union
  (** [Re_union: String_RegLan -> String_RegLan -> String_RegLan]:
      language union. *)
  | Re_inter
  (** [Re_inter: String_RegLan -> String_RegLan -> String_RegLan]:
      language intersection. *)
  | Re_star
  (** [Re_star: String_RegLan -> String_RegLan]: Kleen star. *)
  | Re_cross
  (** [Re_cross: String_RegLan -> String_RegLan]: Kleen cross. *)
  | Re_complement
  (** [Re_complement: String_RegLan -> String_RegLan]: language complement. *)
  | Re_diff
  (** [Re_diff: String_RegLan -> String_RegLan -> String_RegLan]:
      language difference. *)
  | Re_option
  (** [Re_option: String_RegLan -> String_RegLan]: language option
      [Re_option e] abbreviates [Re_union e (Str_to_re "")]. *)
  | Re_power of int
  (** [Re_power(n): String_RegLan -> String_RegLan]:
      [Re_power(n) e] is the nth power of e:
      - [Re_power(0) e] is [Str_to_re ""]
      - [Re_power(n+1) e] is [Re_concat e (Re_power(n) e)] *)
  | Re_loop of int * int
  (** [Re_loop(n1,n2): String_RegLan -> String_RegLan]:
      Defined as:
      - [Re_loop(n₁, n₂) e] is [Re_empty]                   if n₁ > n₂
      - [Re_loop(n₁, n₂) e] is [Re_power(n₁) e]             if n₁ = n₂
      - [Re_loop(n₁, n₂) e] is
        [Re_union ((Re_power(n₁) e) ... (Re_power(n₂) e))]  if n₁ < n₂
  *)

