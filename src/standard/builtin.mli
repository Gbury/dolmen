
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
    - ['a. 'a -> ... -> Prop.T] describes a family of functions that take
      a type and then an arbitrary number of arguments of that type, and
      return a proposition (this is for instance the type of equality).

    Additionally, due to some languages having overloaded operators, and in
    order to not have too verbose names, some of these builtins may have
    overloaded signtures, such as comparisons on numbers which can operate on
    integers, rationals, or reals. Note that arbitrary arity operators (well
    family of operators) can be also be seen as overloaded operators.
    Overloaded types (particularly for numbers) are written:
    - [{a=(Int|Rational|Real)} a -> a -> Prop.T], where the notable difference
      with polymorphic functions is that functions of this type does not
      take a type argument.
*)


(** {2 Type definition} *)
(*  ************************************************************************* *)

type 'a t = ..
constraint 'a = < .. >
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

type _ t +=
  | Unit
  (** The unit type, which has only one element (named void). *)
  | Univ
  (** [Univ: ttype]: a builtin type constant used for languages
      with a default type for elements (such as tptp's `$i`). *)

type _ t +=
  | Equal
  (** [Equal: 'a. 'a -> ... -> Prop.T]: equality beetween values. *)
  | Distinct
  (** [Distinct: 'a. 'a -> ... -> Prop.T]: pairwise dis-equality beetween arguments. *)

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

type _ t +=
  | Multi_trigger
  (** [Multi_trigger: 'a1 ... 'an. 'a1 -> ... -> 'an -> Prop.T]:
      Create a multi trigger: it takes an arbitrarily long list
      of terms of arbitrary types. *)

  | Maps_to
  (** [Maps_to: 'term_var -> 'term -> Prop.T]:
      Used in semantic triggers for floating point arithmetic.
      See [alt-ergo/src/preludes/fpa-theory-2017-01-04-16h00.ae].

      [warning:] It is an Alt-Ergo semantic trigger that should only be
      allowed inside theories. *)

  | Semantic_trigger
  (** [Semantic_trigger: Prop.T -> Prop.T]:
      Denote that its argument is a semantic trigger
      (used only by Alt-ergo currently). *)

type _ t +=
  | Pi
  (** [Pi: 'a. ('a -> Prop.T) -> Prop.T]: higher-order encoding of universal quantification. *)
  | Sigma
  (** [Sigma: 'a. ('a -> Prop.T) -> Prop.T]: higher-order encoding of existencial quantification. *)


(** {2 Boolean Builtins} *)
(*  ************************************************************************* *)

module Prop : sig
  type _ t =
    | T         (** [Prop.T.T: ttype]: the builtin type constant for the type of propositions / booleans. *)
    | True      (** [True: Prop.T]: the [true] proposition. *)
    | False     (** [False: Prop.T]: the [false] proposition. *)
    | Neg       (** [Neg: Prop.T -> Prop.T]: propositional negation. *)
    | And       (** [And: Prop.T -> ... -> Prop.T]: propositional conjunction. *)
    | Or        (** [Or: Prop.T -> ... -> Prop.T]: propositional disjunction. *)
    | Nand      (** [Nand: Prop.T -> Prop.T -> Prop.T]: propositional negated conjunction. *)
    | Nor       (** [Nor: Prop.T -> Prop.T -> Prop.T]: propositional negated disjunction. *)
    | Xor       (** [Xor: Prop.T -> Prop.T -> Prop.T]: ppropositional exclusive disjunction. *)
    | Imply     (** [Imply: Prop.T -> Prop.T -> Prop.T]: propositional implication. *)
    | Implied   (** [Implied: Prop.T -> Prop.T -> Prop.T]: reverse propositional implication. *)
    | Equiv     (** [Equiv: Prop.T -> Prop.T -> Prop.T]: propositional Equivalence. *)
    | Ite       (** [Ite: 'a. Prop.T -> 'a -> 'a -> 'a]: branching operator. *)
end

type 'a t += Prop of 'a Prop.t
(** Boolean builtins. *)


(** {2 Algebraic datatype Builtins} *)
(*  ************************************************************************* *)

module Adt : sig
  type _ t =
    | Tester :
        { adt: 'ty_cst; case: int; cstr : 'term_cst; } ->
        < ty_cst : 'ty_cst ; term_cst : 'term_cst; .. > t
    (** [Tester { adt; cstr; case; }] is the tester of the case-th constructor
        of type [adt] which should be [cstr]. *)
    | Constructor :
        { adt : 'ty_cst; case : int; } ->
        < ty_cst : 'ty_cst ; .. > t
    (** [Constructor { adt; case}] is the case-th constructor of the algebraic
        datatype defined by [adt]. *)
    | Destructor :
        { adt : 'ty_cst; case : int; cstr : 'term_cst; field: int; } ->
        < ty_cst : 'ty_cst ; term_cst : 'term_cst; .. > t
    (** [Destructor { adt; cstr; case; field; }] is the destructor returning the
        field-th argument of the case-th constructor of type [adt] which should
        be [cstr]. *)
end

type 'a t += Adt of 'a Adt.t
(** Alegbraic datatype builtins. *)


(** {2 HOL encoding Builtins} *)
(*  ************************************************************************* *)

module Map : sig
  type _ t =
    | T
    (** [Map.T: ttype -> ttype -> ttype] is the type for encoding higher order
        functions into first order. An [(arg, ret) Map.T] is a first-order type
        for encodings of functions from [arg] to [ret]. *)
    | App
    (** [App: 'arg 'ret. ('arg, 'ret) Map.T -> 'arg -> 'ret]:
        Application operation for encodings of higher-order functions. *)
end

type 'a t += Map of 'a Map.t
(** Map/Higher-order encoding builtins. *)


(** {2 Arithmetic Builtins} *)
(*  ************************************************************************* *)

module Arith : sig
  type _ t =
    | Int
    (** [Int: ttype] the type for signed integers of arbitrary precision. *)
    | Integer of string
    (** [Integer s: Int]: integer literal. The string [s] should be the
        decimal representation of an integer with arbitrary precision (hence
        the use of strings rather than the limited precision [int]). *)
    | Rat
    (** [Rat: ttype] the type for signed rationals. *)
    | Rational of string
    (** [Rational s: Rational]: rational literal. The string [s] should be
        the decimal representation of a rational (see the various languages
        spec for more information). *)
    | Real
    (** [Real: ttype] the type for signed reals. *)
    | Decimal of string
    (** [Decimal s: Real]: real literals. The string [s] should be a
        floating point representation of a real. Not however that reals
        here means the mathematical abstract notion of real numbers, including
        irrational, non-algebric numbers, and is thus not restricted to
        floating point numbers, although these are the only literals
        supported.

        Note that, in spite of the name, the literals may not be expressed in
        decimal notation. For instance, [Decimal "0x1.0p1"] is a valid
        representation for the real number [2].

        Real literals can be parsed using ZArith's [Q.of_string]. *)
    | Lt of [ `Int | `Rat | `Real ]
    (** [Lt: {a=(Int|Rational|Real)} a -> a -> Prop.T]:
        strict comparison (less than) on numbers
        (whether integers, rationals, or reals). *)
    | Leq of [ `Int | `Rat | `Real ]
    (** [Leq:{a=(Int|Rational|Real)} a -> a -> Prop.T]:
        large comparison (less or equal than) on numbers
        (whether integers, rationals, or reals). *)
    | Gt of [ `Int | `Rat | `Real ]
    (** [Gt:{a=(Int|Rational|Real)} a -> a -> Prop.T]:
        strict comparison (greater than) on numbers
        (whether integers, rationals, or reals). *)
    | Geq of [ `Int | `Rat | `Real ]
    (** [Geq:{a=(Int|Rational|Real)} a -> a -> Prop.T]:
        large comparison (greater or equal than) on numbers
        (whether integers, rationals, or reals). *)
    | Minus of [ `Int | `Rat | `Real ]
    (** [Minus:{a=(Int|Rational|Real)} a -> a]:
        arithmetic unary negation/minus on numbers
        (whether integers, rationals, or reals). *)
    | Add of [ `Int | `Rat | `Real ]
    (** [Add:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic addition on numbers
        (whether integers, rationals, or reals). *)
    | Sub of [ `Int | `Rat | `Real ]
    (** [Sub:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic substraction on numbers
        (whether integers, rationals, or reals). *)
    | Mul of [ `Int | `Rat | `Real ]
    (** [Mul:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic multiplication on numbers
        (whether integers, rationals, or reals). *)
    | Pow of [ `Int | `Real ]
    (** [Pow:{a=(Int|Real)} a -> a -> a]:
        arithmetic exponentiation on numbers
        (whether integers or reals). *)
    | Div of [ `Rat | `Real ]
    (** [Div:{a=(Rational|Real)} a -> a -> a]:
        arithmetic exact division on numbers
        (rationals, or reals, but **not** integers). *)
    | Div_e of [ `Int | `Rat | `Real ]
    (** [Div_e:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic integer euclidian quotient
        (whether integers, rationals, or reals).
        If D is positive then [Div_e (N,D)] is the floor
        (in the type of N and D) of the real division [N/D],
        and if D is negative then [Div_e (N,D)] is the ceiling
        of [N/D]. *)
    | Modulo_e of [ `Int | `Rat | `Real ]
    (** [Modulo_e:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic integer euclidian remainder
        (whether integers, rationals, or reals).
        It is defined by the following equation:
        [Div_e (N, D) * D + Modulo(N, D) = N]. *)
    | Div_t of [ `Int | `Rat | `Real ]
    (** [Div_t:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic integer truncated quotient
        (whether integers, rationals, or reals).
        [Div_t (N,D)] is the truncation of the real
        division [N/D]. *)
    | Modulo_t of [ `Int | `Rat | `Real ]
    (** [Modulo_t:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic integer truncated remainder
        (whether integers, rationals, or reals).
        It is defined by the following equation:
        [Div_t (N, D) * D + Modulo_t(N, D) = N]. *)
    | Div_f of [ `Int | `Rat | `Real ]
    (** [Div_f:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic integer floor quotient
        (whether integers, rationals, or reals).
        [Div_t (N,D)] is the floor of the real
        division [N/D]. *)
    | Modulo_f of [ `Int | `Rat | `Real ]
    (** [Modulo_f:{a=(Int|Rational|Real)} a -> a -> a]:
        arithmetic integer floor remainder
        (whether integers, rationals, or reals).
        It is defined by the following equation:
        [Div_f (N, D) * D + Modulo_f(N, D) = N]. *)
    | Abs
    (** [Abs: Int -> Int]:
        absolute value on integers. *)
    | Divisible
    (** [Divisible: Int -> Int -> Prop.T]:
        divisibility predicate on integers. Smtlib restricts
        applications of this predicate to have a litteral integer
        for the divisor/second argument. *)
    | Is_int of [ `Int | `Rat | `Real ]
    (** [Is_int:{a=(Int|Rational|Real)} a -> Prop.T]:
        integer predicate for numbers: is the given number
        an integer. *)
    | Is_rat of [ `Int | `Rat | `Real ]
    (** [Is_rat:{a=(Int|Rational|Real)} a -> Prop.T]:
        rational predicate for numbers: is the given number
        an rational. *)
    | Floor of [ `Int | `Rat | `Real ]
    (** [Floor:{a=(Int|Rational|Real)} a -> a]:
        floor function on numbers, defined in tptp as
        the largest integer not greater than the argument. *)
    | Floor_to_int of [ `Rat | `Real ]
    (** [Floor_to_int:{a=(Rational|Real)} a -> Int]:
        floor and conversion to integers in a single function.
        Should return the greatest integer [i] such that the
        rational or real intepretation of [i] is less than, or
        equal to, the argument. *)
    | Ceiling of [ `Int | `Rat | `Real ]
    (** [Ceiling:{a=(Int|Rational|Real)} a -> a]:
        ceiling function on numbers, defined in tptp as
        the smallest integer not less than the argument. *)
    | Truncate of [ `Int | `Rat | `Real ]
    (** [Truncate:{a=(Int|Rational|Real)} a -> a]:
        ceiling function on numbers, defined in tptp as
        the nearest integer value with magnitude not greater
        than the absolute value of the argument. *)
    | Round of [ `Int | `Rat | `Real ]
    (** [Round:{a=(Int|Rational|Real)} a -> a]:
        rounding function on numbers, defined in tptp as
        the nearest intger to the argument; when the argument
        is halfway between two integers, the nearest even integer
        to the argument. *)
end

type 'a t += Arith of 'a Arith.t
(** Arithmetic builtins. *)


(** {2 Arrays Builtins} *)
(*  ************************************************************************* *)

module Array : sig
  type _ t =
    | T
    (** [Array.T: ttype -> ttype -> ttype]: the type constructor for
        polymorphic functional arrays. An [(src, dst) Array] is an array
        from expressions of type [src] to expressions of type [dst].
        Typically, such arrays are immutables. *)
    | Const
    (** [Store: 'a 'b. 'b -> ('a, 'b) Array.T]: returns a constant array,
        which maps any value of type ['a] to the given base value. *)
    | Store
    (** [Store: 'a 'b. ('a, 'b) Array.T -> 'a -> 'b -> ('a, 'b) Array.T]:
        store operation on arrays. Returns a new array with the key bound
        to the given value (shadowing the previous value associated to
        the key). *)
    | Select
    (** [Select: 'a 'b. ('a, 'b) Array.T -> 'a -> 'b]:
        select operation on arrays. Returns the value associated to the
        given key. Typically, functional arrays are complete, i.e. all
        keys are mapped to a value. *)
end

type 'a t += Array of 'a Array.t
(** Array builtins. *)


(** {2 Bitvectors Builtins} *)
(*  ************************************************************************* *)

module Bitv : sig
  type _ t =
    | T of int
    (** [Bitv.T n: ttype]: type constructor for bitvectors of length [n].
        Ensures that [n > 0]. *)
    | Binary_lit of string
    (** [Lit s: Bitv.T]: bitvector litteral. The string [s] should
        be a binary representation of bitvectors using characters
        ['0'], and ['1'] (lsb last) *)
    | To_int of { n : int; signed : bool; }
    (** [To_int(n,signed): Bitv.T(n) -> Int]:
        conversion from bitvectors to signed integers. *)
    | Of_int of { n : int; }
    (** [Of_int(n): Int -> Bitv.T(n)]:
        conversion fromm (signed) integers to bitvectors. *)
    | Concat of { n : int; m : int }
    (** [Concat(n,m): Bitv.T(n) -> Bitv.T(m) -> Bitv.T(n+m)]:
        concatenation operator on bitvectors. *)
    | Extract of { n : int; i : int; j : int }
    (** [Extract(n, i, j): Bitv.T(n) -> Bitv.T(i - j + 1)]:
        bitvector extraction, from index [j] up to [i] (both included).
        Ensures that [0 <= j <= i < n]. *)
    | Repeat of { n : int; k : int }
    (** [Repeat(n,k): Bitv.T(n) -> Bitv.T(n*k)]:
        bitvector repeatition. *)
    | Zero_extend of { n : int; k : int }
    (** [Zero_extend(n,k): Bitv.T(n) -> Bitv.T(n + k)]:
        zero extension for bitvectors (produces a representation of the
        same unsigned integer). *)
    | Sign_extend of { n : int; k : int }
    (** [Sign_extend(n,k): Bitv.T(n) -> Bitv.T(n + k)]:
        sign extension for bitvectors ((produces a representation of the
        same signed integer). *)
    | Rotate_right of { n : int; i : int }
    (** [Rotate_right(n,i): Bitv.T(n) -> Bitv.T(n)]:
        logical rotate right for bitvectors by [i]. *)
    | Rotate_left of { n : int; i : int }
    (** [Rotate_left(n,i): Bitv.T(n) -> Bitv.T(n)]:
        logical rotate left for bitvectors by [i]. *)
    | Not of int
    (** [not(n): Bitv.T(n) -> Bitv.T(n)]:
        bitwise negation for bitvectors. *)
    | And of int
    (** [and(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        bitwise conjunction for bitvectors. *)
    | Or of int
    (** [bitv_or(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        bitwise disjunction for bitvectors. *)
    | Nand of int
    (** [nand(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        bitwise negated conjunction for bitvectors.
        [Nand s t] abbreviates [Not (And s t))]. *)
    | Nor of int
    (** [Nor(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        bitwise negated disjunction for bitvectors.
        [Nor s t] abbreviates [Not (Or s t))]. *)
    | Xor of int
    (** [Xor(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        bitwise exclusive disjunction for bitvectors.
        [Xor s t] abbreviates
        [Or (And s (Not t)) (And (Not s) t) ]. *)
    | Xnor of int
    (** [Xnor(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        bitwise negated exclusive disjunction for bitvectors.
        [Xnor s t] abbreviates
        [Or (And s t) (And (Not s) (Not t))]. *)
    | Comp of int
    (** [Comp(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(1)]:
        Returns the constant bitvector ["1"] is all bits are equal,
        and the bitvector ["0"] if not. *)
    | Neg of int
    (** [Neg(n): Bitv.T(n) -> Bitv.T(n)]:
        2's complement unary minus. *)
    | Add of int
    (** [Add(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        addition modulo 2^n. *)
    | Sub of int
    (** [Sub(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        2's complement subtraction modulo 2^n. *)
    | Mul of int
    (** [Mul(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        multiplication modulo 2^n. *)
    | Udiv of int
    (** [Udiv(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        unsigned division, truncating towards 0. *)
    | Urem of int
    (** [Urem(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        unsigned remainder from truncating division. *)
    | Sdiv of int
    (** [Sdiv(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        2's complement signed division. *)
    | Srem of int
    (** [Srem(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        2's complement signed remainder (sign follows dividend). *)
    | Smod of int
    (** [Smod(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        2's complement signed remainder (sign follows divisor). *)
    | Shl of int
    (** [Shl(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        shift left (equivalent to multiplication by 2^x where x
        is the value of the second argument). *)
    | Lshr of int
    (** [Lshr(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        logical shift right (equivalent to unsigned division by 2^x,
        where x is the value of the second argument). *)
    | Ashr of int
    (** [Ashr(n): Bitv.T(n) -> Bitv.T(n) -> Bitv.T(n)]:
        Arithmetic shift right, like logical shift right except that
        the most significant bits of the result always copy the most
        significant bit of the first argument. *)
    | Ult of int
    (** [Ult(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for unsigned less-than. *)
    | Ule of int
    (** [ule(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for unsigned less than or equal. *)
    | Ugt of int
    (** [Ugt(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for unsigned greater-than. *)
    | Uge of int
    (** [Uge(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for unsigned greater than or equal. *)
    | Slt of int
    (** [Slt(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for signed less-than. *)
    | Sle of int
    (** [Sle(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for signed less than or equal. *)
    | Sgt of int
    (** [Sgt(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for signed greater-than. *)
    | Sge of int
    (** [Sge(n): Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary predicate for signed greater than or equal. *)
    | Overflow_neg of { n : int; }
    (** [Overflow_neg(n) : Bitv.T(n) -> Prop.T]:
        unary overflow predicate for signed unary minus
        (i.e. returns [true] if the negation would overflow) *)
    | Overflow_add of { n : int; signed : bool; }
    (** [Overflow_add(n,signed) : Bitv.T(n) -> Bitv.T(n) -> Prop.T]:
        binary overflow predicate for signed/unsigned addition
        (i.e. returns [true] if the operation would overflow) *)
    | Overflow_sub of { n : int; signed : bool; }
    (** [Overflow_sub(n,signed): Bitv.T(n) -> Bitv.T(n) -> Prop.T
        binary overflow predicate for signed/unsigned subtraction
        (i.e. returns [true] if the operation would overflow) *)
    | Overflow_mul of { n : int; signed : bool; }
    (** [Overflow_mul(n,signed): Bitv.T(n) -> Bitv.T(n) -> Prop.T
        binary overflow predicate for signed/unsigned multiplication
        (i.e. returns [true] if the operation would overflow) *)
    | Overflow_div of { n : int; }
    (** [Overflow_mul(n,signed): Bitv.T(n) -> Bitv.T(n) -> Prop.T
        binary overflow predicate for signed division
        (i.e. returns [true] if the operation would overflow) *)
end

type 'a t += Bitv of 'a Bitv.t
(** Bitvectors builtins *)


(** {2 Floats Builtins} *)
(*  ************************************************************************* *)

module Float : sig
  type _ t =
    | T of int * int
    (** [Float(e,s): ttype]: type constructor for floating point of exponent of
        size [e] and significand of size [s] (hidden bit included). Those size are
        greater than 1 *)
    | RoundingMode
    (** [RoundingMode: ttype]: type for enumerated type of rounding modes. *)
    | Fp of int * int
    (** [Fp(e, s): Bitv.T(1) -> Bitv.T(e) -> Bitv.T(s-1) -> Fp(e,s)]: bitvector literal.
        The IEEE-format is used for the conversion [sb^se^ss].
        All the NaN are converted to the same value. *)
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
    | Abs  of int * int
    (** [Fp_abs(s,e): Fp(s,e) -> Fp(s,e)]: absolute value *)
    | Neg  of int * int
    (** [Fp_neg(s,e): Fp(s,e) -> Fp(s,e)]: negation *)
    | Add  of int * int
    (** [Fp_add(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: addition *)
    | Sub  of int * int
    (** [Fp_sub(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: subtraction *)
    | Mul  of int * int
    (** [Fp_mul(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: multiplication *)
    | Div  of int * int
    (** [Fp_div(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: division *)
    | Fma  of int * int
    (** [Fp_fma(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e)]: fuse multiply add *)
    | Sqrt  of int * int
    (** [Fp_sqrt(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e)]: square root *)
    | Rem  of int * int
    (** [Fp_rem(s,e): Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: remainder *)
    | RoundToIntegral  of int * int
    (** [Fp_roundToIntegral(s,e): RoundingMode -> Fp(s,e) -> Fp(s,e)]: round to integral *)
    | Min  of int * int
    (** [Fp_min(s,e): Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: minimum *)
    | Max  of int * int
    (** [Fp_max(s,e): Fp(s,e) -> Fp(s,e) -> Fp(s,e)]: maximum *)
    | Leq  of int * int
    (** [Fp_leq(s,e): Fp(s,e) -> Fp(s,e) -> Prop.T]: IEEE less or equal *)
    | Lt  of int * int
    (** [Fp_lt(s,e): Fp(s,e) -> Fp(s,e) -> Prop.T]: IEEE less than *)
    | Geq  of int * int
    (** [Fp_geq(s,e): Fp(s,e) -> Fp(s,e) -> Prop.T]: IEEE greater or equal *)
    | Gt  of int * int
    (** [Fp_gt(s,e): Fp(s,e) -> Fp(s,e) -> Prop.T]: IEEE greater than *)
    | Eq  of int * int
    (** [Fp_eq(s,e): Fp(s,e) -> Fp(s,e) -> Prop.T]: IEEE equality *)
    | IsNormal  of int * int
    (** [Fp_isNormal(s,e): Fp(s,e) -> Prop.T]: test if it is a normal floating point *)
    | IsSubnormal  of int * int
    (** [Fp_isSubnormal(s,e): Fp(s,e) -> Prop.T]: test if it is a subnormal floating point *)
    | IsZero  of int * int
    (** [Fp_isZero(s,e): Fp(s,e) -> Prop.T]: test if it is a zero *)
    | IsInfinite  of int * int
    (** [Fp_isInfinite(s,e): Fp(s,e) -> Prop.T]: test if it is an infinite *)
    | IsNaN  of int * int
    (** [Fp_isNaN(s,e): Fp(s,e) -> Prop.T]: test if it is Not a Number *)
    | IsNegative  of int * int
    (** [Fp_isNegative(s,e): Fp(s,e) -> Prop.T]: test if it is negative *)
    | IsPositive  of int * int
    (** [Fp_isPositive(s,e): Fp(s,e) -> Prop.T]: test if it is positive *)
    | Ieee_format_to_fp of int * int
    (** [Ieee_format_to_fp(s,e): Bv(s+e) -> Fp(s,e)]: Convert from IEEE interchange format *)
    | To_fp of int * int * int * int
    (** [Fp_to_fp(s1,e1,s2,e2): RoundingMode -> Fp(s1,e1) -> Fp(s2,e2)]:
        Convert from another floating point format *)
    | Of_real of int * int
    (** [Real_to_fp(s,e): RoundingMode -> Real -> Fp(s,e)]: Convert from a real *)
    | Of_sbv of int * int * int
    (** [Sbv_to_fp(m,s,e): RoundingMode -> Bitv.T(m) -> Fp(s,e)]: Convert from a signed integer *)
    | Of_ubv of int * int * int
    (** [Ubv_to_fp(m,s,e): RoundingMode -> Bitv.T(m) -> Fp(s,e)]: Convert from a unsigned integer *)
    | To_ubv of int * int * int
    (** [To_ubv(s,e,m): RoundingMode -> Fp(s,e) -> Bitv.T(m)]: Convert to an unsigned integer *)
    | To_sbv of int * int * int
    (** [To_ubv(s,e,m): RoundingMode -> Fp(s,e) -> Bitv.T(m)]: Convert to an signed integer *)
    | To_real of int * int
    (** [To_real(s,e): Fp(s,e) -> Real]: Convert to real *)
end

type 'a t += Float of 'a Float.t


(** {2 String and Regexp Builtins} *)
(*  ************************************************************************* *)

module Str : sig

  type _ t =
    | T
    (** [String: ttype]: type constructor for strings. *)
    | Raw of string
    (** [Str s: String]: string literals. *)
    | Length
    (** [Str_length: String -> Int]: string length. *)
    | At
    (** [Str_at: String -> Int -> String]:
        Singleton string containing a character at given position
        or empty string when position is out of range.
        The leftmost position is 0. *)
    | To_code
    (** [Str_to_code: String -> Int]:
        [Str_to_code s] is the code point of the only character of s,
        if s is a singleton string; otherwise, it is -1. *)
    | Of_code
    (** [Str_of_code: Int -> String]:
        [Str_of_code n] is the singleton string whose only character is
        code point n if n is in the range [0, 196607]; otherwise, it is the
        empty string. *)
    | Is_digit
    (** [Str_is_digit: String -> Prop.T]: Digit check
        [Str.is_digit s] is true iff s consists of a single character which is
        a decimal digit, that is, a code point in the range 0x0030 ... 0x0039. *)
    | To_int
    (** [Str_to_int: String -> Int]: Conversion to integers
        [Str.to_int s] with s consisting of digits (in the sense of str.is_digit)
        evaluates to the positive integer denoted by s when seen as a number in
        base 10 (possibly with leading zeros).
        It evaluates to -1 if s is empty or contains non-digits. *)
    | Of_int
    (** [Str_of_int : Int -> String]: Conversion from integers.
        [Str.from_int n] with n non-negative is the corresponding string in
        decimal notation, with no leading zeros. If n < 0, it is the empty string. *)
    | Concat
    (** [Str_concat: String -> String -> String]: string concatenation. *)
    | Sub
    (** [Str_sub: String -> Int -> Int -> String]:
        [Str_sub s i n] evaluates to the longest (unscattered) substring
        of s of length at most n starting at position i.
        It evaluates to the empty string if n is negative or i is not in
        the interval [0,l-1] where l is the length of s. *)
    | Index_of
    (** [Str_index_of: String -> String -> Int -> Int]:
        Index of first occurrence of second string in first one starting at
        the position specified by the third argument.
        [Str_index_of s t i], with 0 <= i <= |s| is the position of the first
        occurrence of t in s at or after position i, if any.
        Otherwise, it is -1. Note that the result is i whenever i is within
        the range [0, |s|] and t is empty. *)
    | Replace
    (** [Str_replace: String -> String -> String -> String]: Replace
        [Str_replace s t t'] is the string obtained by replacing the first
        occurrence of t in s, if any, by t'. Note that if t is empty, the
        result is to prepend t' to s; also, if t does not occur in s then
        the result is s. *)
    | Replace_all
    (** [Str_replace_all: String -> String -> String -> String]:
        [Str_replace_all s t t’] is s if t is the empty string. Otherwise, it
        is the string obtained from s by replacing all occurrences of t in s
        by t’, starting with the first occurrence and proceeding in
        left-to-right order. *)
    | Replace_re
    (** [Str_replace_re: String -> String_RegLan -> String -> String]:
        [Str_replace_re s r t] is the string obtained by replacing the
        shortest leftmost non-empty match of r in s, if any, by t.
        Note that if t is empty, the result is to prepend t to s. *)
    | Replace_re_all
    (** [Str_replace_re_all: String -> String_RegLan -> String -> String]:
        [Str_replace_re_all s r t] is the string obtained by replacing,
        left-to right, each shortest *non-empty* match of r in s by t. *)
    | Is_prefix
    (** [Str_is_prefix: String -> String -> Prop.T]: Prefix check
        [Str_is_prefix s t] is true iff s is a prefix of t. *)
    | Is_suffix
    (** [Str_is_suffix: String -> String -> Prop.T]: Suffix check
        [Str_is_suffix s t] is true iff s is a suffix of t. *)
    | Contains
    (** [Str_contains: String -> String -> Prop.T]: Inclusion check
        [Str_contains s t] is true iff s contains t. *)
    | Lexicographic_strict
    (** [Str_lexicographic_strict: String -> String -> Prop.T]:
        lexicographic ordering (strict). *)
    | Lexicographic_large
    (** [Str_lexicographic_large: String -> String -> Prop.T]:
        reflexive closure of the lexicographic ordering. *)
    | In_re
    (** [Str_in_re: String -> String_RegLan -> Prop.T]: set membership. *)

  module RegLan : sig
    (* String Regular languages *)
    type _ t =
      | T
      (** [String_RegLan: ttype]:
          type constructor for Regular languages over strings. *)
      | Empty
      (** [Re_empty: String_RegLan]:
          the empty language. *)
      | All
      (** [Re_all: String_RegLan]:
          the language of all strings. *)
      | Allchar
      (** [Re_allchar: String_RegLan]:
          the language of all singleton strings. *)
      | Of_string
      (** [Re_of_string: String -> String_RegLan]:
          the singleton language with a single string. *)
      | Range
      (** [Re_range: String -> String -> String_RegLan]: Language range
          [Re_range s1 s2] is the set of all *singleton* strings [s] such that
          [Str_lexicographic_large s1 s s2] provided [s1] and [s1] are singleton.
          Otherwise, it is the empty language. *)
      | Concat
      (** [Re_concat: String_RegLan -> String_RegLan -> String_RegLan]:
          language concatenation. *)
      | Union
      (** [Re_union: String_RegLan -> String_RegLan -> String_RegLan]:
          language union. *)
      | Inter
      (** [Re_inter: String_RegLan -> String_RegLan -> String_RegLan]:
          language intersection. *)
      | Star
      (** [Re_star: String_RegLan -> String_RegLan]: Kleen star. *)
      | Cross
      (** [Re_cross: String_RegLan -> String_RegLan]: Kleen cross. *)
      | Complement
      (** [Re_complement: String_RegLan -> String_RegLan]: language complement. *)
      | Diff
      (** [Re_diff: String_RegLan -> String_RegLan -> String_RegLan]:
          language difference. *)
      | Option
      (** [Re_option: String_RegLan -> String_RegLan]: language option
          [Re_option e] abbreviates [Re_union e (Str_to_re "")]. *)
      | Power of int
      (** [Re_power(n): String_RegLan -> String_RegLan]:
          [Re_power(n) e] is the nth power of e:
          - [Re_power(0) e] is [Str_to_re ""]
          - [Re_power(n+1) e] is [Re_concat e (Re_power(n) e)] *)
      | Loop of int * int
      (** [Re_loop(n1,n2): String_RegLan -> String_RegLan]:
          Defined as:
          - [Re_loop(n₁, n₂) e] is [Re_empty]                   if n₁ > n₂
          - [Re_loop(n₁, n₂) e] is [Re_power(n₁) e]             if n₁ = n₂
          - [Re_loop(n₁, n₂) e] is
            [Re_union ((Re_power(n₁) e) ... (Re_power(n₂) e))]  if n₁ < n₂
      *)
  end

end

type 'a t +=
  | Str of 'a Str.t
  (** String builtins *)
  | Regexp of 'a Str.RegLan.t
  (** String Regular language builtins, aka regexps. *)

