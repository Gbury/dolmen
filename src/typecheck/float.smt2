(theory FloatingPoint

 :smt-lib-version 2.6
 :smt-lib-release "2017-11-24"
 :written-by "Cesare Tinelli and Martin Brain"
 :date "2014-05-27"
 :last-updated "2015-04-25"
 :update-history
 "Note: history only accounts for content changes, not release changes.
  2015-04-25 Updated to Version 2.5.
             Updated reference to tech report.
 "

 :notes
 "This is a theory of floating point numbers largely based on the IEEE standard
  754-2008 for floating-point arithmetic (http://grouper.ieee.org/groups/754/)
  but restricted to the binary case only.
  A major extension over 754-2008 is that the theory has a sort for every
  possible exponent and significand length.

  Version 1 of the theory was based on proposal by P. Ruemmer and T. Wahl [RW10].

  [RW10] Philipp Ruemmer and Thomas Wahl.
         An SMT-LIB Theory of Binary Floating-Point Arithmetic.
         Proceedings of the 8th International Workshop on
         Satisfiability Modulo Theories (SMT'10), Edinburgh, UK, July 2010.
         (http://www.philipp.ruemmer.org/publications/smt-fpa.pdf)

  Version 2 was written by C. Tinelli using community feedback.
  Version 3, the current one, was written by C. Tinelli and M. Brain following
  further discussion within the SMT-LIB community, and then relaborated with
  P. Ruemmer and T. Wahl.
  A more detailed description of this version together with the rationale of
  several models decisions as well as a formal semantics of the theory can be
  found in

  [BTRW15] Martin Brain, Cesare Tinelli, Philipp Ruemmer, and Thomas Wahl.
           An Automatable Formal Semantics for IEEE-754 Floating-Point Arithmetic
           Technical Report, 2015.
           (http://smt-lib.org/papers/BTRW15.pdf)

  The following additional people provided substantial feedback and directions:
  François Bobot, David Cok, Alberto Griggio, Florian Lapschies, Leonardo de
  Moura, Gabriele Paganelli, Cody Roux, Christoph Wintersteiger.
 "

;-------
; Sorts
;-------

 :sorts ((RoundingMode 0) (Real 0))

 ; Bit vector sorts, indexed by vector size
 :sorts_description "All sort symbols of the form
    (_ BitVec m)
  where m is a numeral greater than 0."

 ; Floating point sort, indexed by the length of the exponent and significand
 ; components of the number
 :sorts_description "All nullary sort symbols of the form

    (_ FloatingPoint eb sb),

  where eb and sb are numerals greater than 1."

 :notes
 "eb defines the number of bits in the exponent;
  sb defines the number of bits in the significand, *including* the hidden bit.
 "

; Short name for common floating point sorts
:sort ((Float16 0) (Float32 0) (Float64 0) (Float128 0))

 :notes "
  -  Float16 is a synonym for (_ FloatingPoint  5  11)
  -  Float32 is a synonym for (_ FloatingPoint  8  24)
  -  Float64 is a synonym for (_ FloatingPoint 11  53)
  - Float128 is a synonym for (_ FloatingPoint 15 113)

  These correspond to the IEEE binary16, binary32, binary64 and binary128 formats.
 "

;----------------
; Rounding modes
;----------------

 ; Constants for rounding modes, and their abbreviated version
 :funs ((roundNearestTiesToEven RoundingMode) (RNE RoundingMode)
        (roundNearestTiesToAway RoundingMode) (RNA RoundingMode)
        (roundTowardPositive RoundingMode)    (RTP RoundingMode)
        (roundTowardNegative RoundingMode)    (RTN RoundingMode)
        (roundTowardZero RoundingMode)        (RTZ RoundingMode)
        )

;--------------------
; Value constructors
;--------------------

 ; Bitvector literals
 :funs_description "
    All binaries #bX of sort (_ BitVec m) where m is the number of digits in X.
    All hexadecimals #xX of sort (_ BitVec m) where m is 4 times the number of
    digits in X.
 "

 ; FP literals as bit string triples, with the leading bit for the significand
 ; not represented (hidden bit)
 :funs_description "All function symbols with declaration of the form

   (fp (_ BitVec 1) (_ BitVec eb) (_ BitVec i) (_ FloatingPoint eb sb))

   where eb and sb are numerals greater than 1 and i = sb - 1."

 ; Plus and minus infinity
 :funs_description "All function symbols with declaration of the form

   ((_ +oo eb sb) (_ FloatingPoint eb sb))
   ((_ -oo eb sb) (_ FloatingPoint eb sb))

  where eb and sb are numerals greater than 1."

 :notes
 "Semantically, for each eb and sb, there is exactly one +infinity value and
  exactly one -infinity value in the set denoted by (_ FloatingPoint eb sb),
  in agreement with the IEEE 754-2008 standard.
  However, +/-infinity can have two representations in this theory.
  E.g., +infinity for sort (_ FloatingPoint 2 3) is represented equivalently
  by (_ +oo 2 3) and (fp #b0 #b11 #b00).
 "

 ; Plus and minus zero
 :funs_description "All function symbols with declaration of the form

   ((_ +zero eb sb) (_ FloatingPoint eb sb))
   ((_ -zero eb sb) (_ FloatingPoint eb sb))

  where eb and sb are numerals greater than 1."

 :note
 "The +zero and -zero symbols are abbreviations for the corresponding fp literals.
  E.g.,   (_ +zero 2 4) abbreviates (fp #b0 #b00 #b000)
          (_ -zero 3 2) abbreviates (fp #b1 #b000 #b0)
 "

 ; Non-numbers
 :funs_description "All function symbols with declaration of the form

   ((_ NaN eb sb) (_ FloatingPoint eb sb))

  where eb and sb are numerals greater than 1."

 :notes
 "For each eb and sb, there is exactly one NaN in the set denoted by
  (_ FloatingPoint eb sb), in agreeement with Level 2 of IEEE 754-2008
  (floating-point data). There is no distinction in this theory between
  a ``quiet'' and a ``signaling'' NaN.
  NaN has several representations, e.g.,(_ NaN eb sb) and any term of
  the form (fp t #b1..1 s) where s is a binary containing at least a 1
  and t is either #b0 or #b1.
 "

;-----------
; Operators
;-----------

 :funs_description "All function symbols with declarations of the form below
   where eb and sb are numerals greater than 1.

   ; absolute value
   (fp.abs (_ FloatingPoint eb sb) (_ FloatingPoint eb sb))

   ; negation (no rounding needed)
   (fp.neg (_ FloatingPoint eb sb) (_ FloatingPoint eb sb))

   ; addition
   (fp.add RoundingMode (_ FloatingPoint eb sb) (_ FloatingPoint eb sb)
     (_ FloatingPoint eb sb))

   ; subtraction
   (fp.sub RoundingMode (_ FloatingPoint eb sb) (_ FloatingPoint eb sb)
     (_ FloatingPoint eb sb))

   ; multiplication
   (fp.mul RoundingMode (_ FloatingPoint eb sb) (_ FloatingPoint eb sb)
     (_ FloatingPoint eb sb))

   ; division
   (fp.div RoundingMode (_ FloatingPoint eb sb) (_ FloatingPoint eb sb)
     (_ FloatingPoint eb sb))

   ; fused multiplication and addition; (x * y) + z
   (fp.fma RoundingMode (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) (_ FloatingPoint eb sb)
     (_ FloatingPoint eb sb))

   ; square root
   (fp.sqrt RoundingMode (_ FloatingPoint eb sb) (_ FloatingPoint eb sb))

   ; remainder: x - y * n, where n in Z is nearest to x/y
   (fp.rem (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) (_ FloatingPoint eb sb))

   ; rounding to integral
   (fp.roundToIntegral RoundingMode (_ FloatingPoint eb sb) (_ FloatingPoint eb sb))

   ; minimum and maximum
   (fp.min (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) (_ FloatingPoint eb sb))
   (fp.max (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) (_ FloatingPoint eb sb))

   ; comparison operators
   ; Note that all comparisons evaluate to false if either argument is NaN
   (fp.leq (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) Bool :chainable)
   (fp.lt  (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) Bool :chainable)
   (fp.geq (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) Bool :chainable)
   (fp.gt  (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) Bool :chainable)

   ; IEEE 754-2008 equality (as opposed to SMT-LIB =)
   (fp.eq (_ FloatingPoint eb sb) (_ FloatingPoint eb sb) Bool :chainable)

   ; Classification of numbers
   (fp.isNormal (_ FloatingPoint eb sb) Bool)
   (fp.isSubnormal (_ FloatingPoint eb sb) Bool)
   (fp.isZero (_ FloatingPoint eb sb) Bool)
   (fp.isInfinite (_ FloatingPoint eb sb) Bool)
   (fp.isNaN (_ FloatingPoint eb sb) Bool)
   (fp.isNegative (_ FloatingPoint eb sb) Bool)
   (fp.isPositive (_ FloatingPoint eb sb) Bool)
 "

 :note
 "(fp.eq x y) evaluates to true if x evaluates to -zero and y to +zero, or vice versa.
  fp.eq and all the other comparison operators evaluate to false if one of their
  arguments is NaN.
 "


;------------------------------
; Conversions from other sorts
;------------------------------

 :funs_description "All function symbols with declarations of the form below
   where m is a numerals greater than 0 and eb, sb, mb and nb are numerals
   greater than 1.

   ; from single bitstring representation in IEEE 754-2008 interchange format,
   ; with m = eb + sb
   ((_ to_fp eb sb) (_ BitVec m) (_ FloatingPoint eb sb))

   ; from another floating point sort
   ((_ to_fp eb sb) RoundingMode (_ FloatingPoint mb nb) (_ FloatingPoint eb sb))

   ; from real
   ((_ to_fp eb sb) RoundingMode Real (_ FloatingPoint eb sb))

   ; from signed machine integer, represented as a 2's complement bit vector
   ((_ to_fp eb sb) RoundingMode (_ BitVec m) (_ FloatingPoint eb sb))

   ; from unsigned machine integer, represented as bit vector
   ((_ to_fp_unsigned eb sb) RoundingMode (_ BitVec m) (_ FloatingPoint eb sb))
 "


;----------------------------
; Conversions to other sorts
;----------------------------

 :funs_description "All function symbols with declarations of the form below
   where m is a numeral greater than 0 and  eb and sb are numerals greater than 1.

   ; to unsigned machine integer, represented as a bit vector
   ((_ fp.to_ubv m) RoundingMode (_ FloatingPoint eb sb) (_ BitVec m))

   ; to signed machine integer, represented as a 2's complement bit vector
   ((_ fp.to_sbv m) RoundingMode (_ FloatingPoint eb sb) (_ BitVec m))

   ; to real
   (fp.to_real (_ FloatingPoint eb sb) Real)
 "
 :notes
 "All fp.to_* functions are unspecified for NaN and infinity input values.
  In addition, fp.to_ubv and fp.to_sbv are unspecified for finite number inputs
  that are out of range (which includes all negative numbers for fp.to_ubv).

  This means for instance that the formula

    (= (fp.to_real (_ NaN 8 24)) (fp.to_real (fp c1 c2 c3)))

  is satisfiable in this theory for all binary constants c1, c2, and c3
  (of the proper sort).
 "

 :note
 "There is no function for converting from (_ FloatingPoint eb sb) to the
  corresponding IEEE 754-2008 binary format, as a bit vector (_ BitVec m) with
  m = eb + sb, because (_ NaN eb sb) has multiple, well-defined representations.
  Instead, an encoding of the kind below is recommended, where f is a term
  of sort (_ FloatingPoint eb sb):

   (declare-fun b () (_ BitVec m))
   (assert (= ((_ to_fp eb sb) b) f))
 "

;--------
; Values
;--------

 :values "For all m,n > 1, the values of sort (_ FloatingPoint m n) are
  - (_ +oo m n)
  - (_ -oo m n)
  - (_ NaN m n)
  - all terms of the form (fp c1 c2 c3) where
    - c1 is the binary #b0 or #b1
    - c2 is a binary of size m other than #b1...1 (all 1s)
    - c3 is a binary of size n-1

  The set of values for RoundingMode is {RNE, RNA, RTP, RTN, RTZ}.
 "

 :notes
 "No values are specified for the sorts Real and (_ BitVec n) in this theory.
  They are specified in the theory declarations Reals and FixedSizeBitVectors,
  respectively.
 "

;-----------
; Semantics
;-----------

 :note
 "The semantics of this theory is described somewhat informally here.
  A rigorous, self-contained specification can be found in [BTRW14]:
   'An Automatable Formal Semantics for IEEE-754 Floating-Point Arithmetic'
  and it takes precedence in the case of any (unintended) inconsistencies.
 "

 :definition
 "For every expanded signature Sigma, the instance of FloatingPoints with
  that signature is the theory consisting of all Sigma-models that satisfy
  the constraints detailed below.

  We use [[ _ ]] to denote the meaning of a sort or function symbol in
  a given Sigma-model.


  o (_ FloatingPoint eb sb)

    [[(_ FloatingPoint eb sb)]] is the set of all the binary floating point
    numbers with eb bits for the exponent and sb bits for the significand,
    as defined by IEEE 754-2008.

    Technically, [[(_ FloatingPoint eb sb)]] is the union of the set
    {not_a_number} with four sets N, S, Z, I of bit-vector triples
    (corresponding to normal numbers, subnormal numbers, zeros and
    infinities) of the form (s, e, m) where s, e, and m correspond
    respectively to the sign, the exponent and the significand (see
    the paper for more details).

    Note that the (semantic) value not_a_number is shared across all
    [[(_ FloatingPoint eb sb)]].


  o (_ BitVec m), binary and hexadecimal constants

    These are interpreted as in the theory FixedSizeBitVectors.


  o Real

    [[Real]] is the set of real numbers.


  o RoundingMode

    [[RoundingMode]] is the set of the 5 rounding modes defined by IEEE 754-2008.


  o (roundNearestTiesToEven RoundingMode), (roundNearestTiesToAway RoundingMode), ...

    [[roundNearestTiesToEven]], [[roundNearestTiesToAway]], [[roundTowardPositive]],
    [[roundTowardNegative]], and [[roundTowardZero]] are the 5 distinct elements
    of [[RoundingMode]], and each corresponds to the rounding mode suggested by
    the symbol's name.


  o (RNE RoundingMode), (RNA RoundingMode), ...

    [[RNE]] = [[roundNearestTiesToEven]];
    [[RNA]] = [[roundNearestTiesToAway]];
    [[RTP]] = [[roundTowardPositive]];
    [[RTN]] = [[roundTowardNegative]];
    [[RTZ]] = [[roundTowardZero]].


  o (fp (_ BitVec 1) (_ BitVec eb) (_ BitVec i) (_ FloatingPoint eb sb))

    [[fp]] returns the element of [[(_ FloatingPoint eb sb)]] whose IEEE 754-2008
    binary encoding matches the input bit strings:
    for all bitvectors
    b1 in [[(_ BitVec 1)]], b2 in [[(_ BitVec eb)]] and b3 in [[(_ BitVec i)]],
    [[fp]](b1, b2 ,b3) is the binary floating point number encoded in the IEEE
    754-2008 standard with sign bit b1, exponent bits b2, and significant bit b3
    (with 1 hidden bit).

    Note that not_a_number can be denoted with fp terms as well. For instance, in
    (_ FloatingPoint 2 2),
    [[(_ NaN 2 2)]] = [[fp]]([[#b0]], [[#b11]], [[#b1]])
                    = [[fp]]([[#b1]], [[#b11]], [[#b1]])

    Similarly,
    [[(_ +oo 2 2)]] = [[fp]]([[#b0]], [[#b11]], [[#b0]])
    [[(_ -oo 2 2)]] = [[fp]]([[#b1]], [[#b11]], [[#b0]])


  o ((_ +oo eb sb) (_ FloatingPoint eb sb))
    ((_ -oo eb sb) (_ FloatingPoint eb sb))
    ((_ NaN eb sb) (_ FloatingPoint eb sb))
    ((_ +zero eb sb) (_ FloatingPoint eb sb))
    ((_ -zero eb sb) (_ FloatingPoint eb sb))

    [[(_ +oo eb sb)]] is +infinity
    [[(_ -oo eb sb)]] is -infinity
    [[(_ NaN eb sb)]] is not_a_number
    [[(_ +zero eb sb)]] is [[fp]]([[#b0]], [[#b0..0]], [[#b0..0]]) where
                           the first bitvector literal has eb 0s and
                           the second has sb - 1 0s
    [[(_ -zero eb sb)]] is [[fp]]([[#b1]], [[#b0..0]], [[#b0..0]]) where
                           the first bitvector literal has eb 0s and
                           the second has sb - 1 0s


  o ((_ to_fp eb sb) (_ BitVec m) (_ FloatingPoint eb sb))

    [[(_ to_fp eb sb)]](b) = [[fp]](b[m-1:m-1], b[eb+sb-1:sb], b[sb-1:0])
    where b[p:q] denotes the subvector of bitvector b between positions p and q.


  o (fp.to_real (_ FloatingPoint eb sb) Real)

    [[fp.to_real]](x) is the real number represented by x if x is not in
    {-infinity, -infinity, not_a_number}. Otherwise, it is unspecified.


  o ((_ to_fp eb sb) RoundingMode (_ FloatingPoint m n) (_ FloatingPoint eb sb))

    [[(_ to_fp eb sb)]](r, x) = x if x in {+infinity, -infinity, not_a_number}.
    [[(_ to_fp eb sb)]](r, x) = +/-infinity if [[fp.to_real]](x) is too large/too
    small to be represented as a finite number of [[(_ FloatingPoint eb sb)]];
    [[(_ to_fp eb sb)]](r, x) = y otherwise, where y is the finite number
    such that [[fp.to_real]](y) is closest to [[fp.to_real]](x) according to
    rounding mode r.


  o ((_ to_fp eb sb) RoundingMode Real (_ FloatingPoint eb sb))

    [[(_ to_fp eb sb)]](r, x) = +/-infinity if x is too large/too small
    to be represented as a finite number of [[(_ FloatingPoint eb sb)]];
    [[(_ to_fp eb sb)]](r, x) = y otherwise, where y is the finite number
    such that [[fp.to_real]](y) is closest to x according to rounding mode r.


  o ((_ to_fp eb sb) RoundingMode (_ BitVec m) (_ FloatingPoint eb sb))

    Let b in [[(_ BitVec m)]] and let n be the signed integer represented by b
    (in 2's complement format).
    [[(_ to_fp eb sb)]](r, b) = +/-infinity if n is too large/too small to be
    represented as a finite number of [[(_ FloatingPoint eb sb)]];
    [[(_ to_fp eb sb)]](r, x) = y otherwise, where y is the finite number
    such that [[fp.to_real]](y) is closest to n according to rounding mode r.


  o ((_ to_fp_unsigned eb sb) RoundingMode (_ BitVec m) (_ FloatingPoint eb sb))

    Let b in [[(_ BitVec m)]] and let n be the unsigned integer represented by b.
    [[(_ to_fp_unsigned eb sb)]](r, x) = +infinity if n is too large to be
    represented as a finite number of [[(_ FloatingPoint eb sb)]];
    [[(_ to_fp_unsigned eb sb)]](r, x) = y otherwise, where y is the finite number
    such that [[fp.to_real]](y) is closest to n according to rounding mode r.


  o ((_ fp.to_ubv m) RoundingMode (_ FloatingPoint eb sb) (_ BitVec m))

    [[(_ fp.to_ubv m)]](r, x) = b if the unsigned integer n represented by b is
    the closest integer according to rounding mode r to the real number
    represented by x, and n is in the range [0, 2^m - 1].
    [[(_ fp.to_ubv m)]](r, x) is unspecified in all other cases (including when
    x is in {-infinity, -infinity, not_a_number}).


  o ((_ fp.to_sbv m) RoundingMode (_ FloatingPoint eb sb) (_ BitVec m))

    [[(_ fp.to_sbv m)]](r, x) = b if the signed integer n represented by b
    (in 2's complement format) is the closest integer according to rounding mode
    r to the real number represented by x, and n is in the range
    [-2^{m-1}, 2^{m-1} - 1].
    [[(_ fp.to_sbv m)]](r, x) is unspecified in all other cases (including when
    x is in {-infinity, -infinity, not_a_number}).


  o (fp.isNormal (_ FloatingPoint eb sb) Bool)

    [[fp.isNormal]](x) = true iff x is a normal number.


  o (fp.isSubnormal (_ FloatingPoint eb sb) Bool)

    [[fp.isSubnormal]](x) = true iff x is a subnormal number.


  o (fp.isZero (_ FloatingPoint eb sb) Bool)

    [[fp.isZero]](x) = true iff x is positive or negative zero.


  o (fp.isInfinite (_ FloatingPoint eb sb) Bool)

    [[fp.isInfinite]](x) = true iff x is +infinity or -infinity.


  o (fp.isNaN (_ FloatingPoint eb sb) Bool)

    [[fp.isNaN]](x) = true iff x = not_a_number.


  o (fp.isNegative (_ FloatingPoint eb sb) Bool)

    [[fp.isNegative]](x) = true iff x is [[-zero]] or [[fp.lt]](x, [[-zero]]) holds.

  o (fp.isPositive (_ FloatingPoint eb sb) Bool)

    [[fp.isPositive]](x) = true iff x is [[+zero]] or [[fp.lt]]([[+zero]], x) holds.


  o all the other function symbols are interpreted as described in [BTRW15].
 "
)
