(theory Ints

 :smt-lib-version 2.6
 :smt-lib-release "2017-11-24"
 :written-by "Cesare Tinelli"
 :date "2010-04-17"
 :last-updated "2015-04-25"
 :update-history
 "Note: history only accounts for content changes, not release changes.
  2015-04-25 Updated to Version 2.5.
 "

 :sorts ((Int 0))

 :funs ((NUMERAL Int)
        (- Int Int)                 ; negation
        (- Int Int Int :left-assoc) ; subtraction
        (+ Int Int Int :left-assoc)
        (* Int Int Int :left-assoc)
        (div Int Int Int :left-assoc)
        (mod Int Int Int)
        (abs Int Int)
        (<= Int Int Bool :chainable)
        (<  Int Int Bool :chainable)
        (>= Int Int Bool :chainable)
        (>  Int Int Bool :chainable)
       )

 :funs_description
 "All ranked function symbols of the form
    ((_ divisible n) Int Bool)
  where n is a positive numeral.
 "

 :values
 "The set of values for the sort Int consists of
  - all numerals,
  - all terms of the form (- n) where n is a numeral other than 0.
 "

 :definition
 "For every expanded signature, the instance of Ints with that
  signature is the theory consisting of all Sigma-models that interpret:

  - the sort Int as the set of all integer numbers,

  - each numeral as the corresponding natural number,

  - (_ divisible n) as the function mapping to true all and only
    the integers that are divisible by n,

  - abs as the absolute value function,

  - div and mod according to Boute's Euclidean definition [1], that is,
    so as to satify the formula

    (for all ((m Int) (n Int))
      (=> (distinct n 0)
          (let ((q (div m n)) (r (mod m n)))
            (and (= m (+ (* n q) r))
                 (<= 0 r (- (abs n) 1))))))

  - the other function symbols of Ints as expected.

  References:
  [1] Boute, Raymond T. (April 1992).
      The Euclidean definition of the functions div and mod.
      ACM Transactions on Programming Languages and Systems (TOPLAS)
      ACM Press. 14 (2): 127 - 144. doi:10.1145/128861.128862.
 "

 :notes
 "Regardless of sign of m,
  when n is positive, (div m n) is the floor of the rational number m/n;
  when n is negative, (div m n) is the ceiling of m/n.

  This contrasts with alternative but less robust definitions of div and mod
  where (div m n) is
  - always the integer part of m/n (rounding towards 0), or
  - always the floor of x/y (rounding towards -infinity).
 "

 :notes
 "See note in the Reals theory declaration about terms of the form (/ t 0).
  The same observation applies here to terms of the form (div t 0) and
  (mod t 0).
 "
)

(theory Reals

 :smt-lib-version 2.6
 :smt-lib-release "2017-11-24"
 :written-by "Cesare Tinelli"
 :date "2010-04-17"
 :last-updated "2017-05-08"
 :update-history
 "Note: history only accounts for content changes, not release changes.
  2017-05-08 Fixed error in note on intepretation of (/t 0).
  2016-04-20 Minor formatting of notes fields.
  2015-04-25 Updated to Version 2.5.
  2012-06-20 Modified the definition of :value attribute to include abstract values
             for irrational algebraic numbers.
 "
 :sorts ((Real 0))

 :funs ((NUMERAL Real)
        (DECIMAL Real)
        (- Real Real)                  ; negation
        (- Real Real Real :left-assoc) ; subtraction
        (+ Real Real Real :left-assoc)
        (* Real Real Real :left-assoc)
        (/ Real Real Real :left-assoc)
        (<= Real Real Bool :chainable)
        (<  Real Real Bool :chainable)
        (>= Real Real Bool :chainable)
        (>  Real Real Bool :chainable)
       )

 :values
 "The set of values for the sort Real consists of
  - an abstract value for each irrational algebraic number
  - all numerals
  - all terms of the form (- n) where n is a numeral other than 0
  - all terms of the form (/ m n) or (/ (- m) n) where
    - m is a numeral other than 0,
    - n is a numeral other than 0 and 1,
    - as integers, m and n have no common factors besides 1.
 "
 :definition
 "For every expanded signature Sigma, the instance of Reals with that
  signature is the theory consisting of all Sigma-models that interpret

  - the sort Real as the set of all real numbers,

  - each numeral as the corresponding real number,

  - each decimal as the corresponding real number,

  - / as a total function that coincides with the real division function
    for all inputs x and y where y is non-zero,

  - the other function symbols of Reals as expected.
 "

 :notes
 "Since in SMT-LIB logic all function symbols are interpreted as total
  functions, terms of the form (/ t 0) *are* meaningful in every
  instance of Reals. However, the declaration imposes no constraints
  on their value. This means in particular that
  - for every instance theory T and
  - for every value v (as defined in the :values attribute) and
    closed term t of sort Real,
  there is a model of T that satisfies (= v (/ t 0)).
 "

 :notes
 "The restriction of Reals over the signature having just the symbols
  (0 Real)
  (1 Real)
  (- Real Real)
  (+ Real Real Real)
  (* Real Real Real)
  (<= Real Real Bool)
  (<  Real Real Bool)
  coincides with the theory of real closed fields, axiomatized by
  the formulas below:

   - associativity of +
   (forall ((x Real) (y Real) (z Real))
    (= (+ (+ x y) z) (+ x (+ y z))))

   - commutativity of +
   (forall ((x Real) (y Real))
    (= (* x y) (* y x)))

   - 0 is the right (and by commutativity, left) unit of +
   (forall ((x Real)) (= (+ x 0) x))

   - right (and left) inverse wrt +
   (forall ((x Real)) (= (+ x (- x)) 0))

   - associativity of *
   (forall ((x Real) (y Real) (z Real))
    (= (* (* x y) z) (* x (* y z))))

   - commutativity of *
   (forall ((x Real) (y Real)) (= (* x y) (* y x)))

   - 1 is the right (and by commutativity, left) unit of *
   (forall ((x Real)) (= (* x 1) x))

   - existence of right (and left) inverse wrt *
   (forall ((x Real))
    (or (= x 0) (exists (y Real) (= (* x y) 1))))

   - left distributivity of * over +
   (forall ((x Real) (y Real) (z Real))
    (= (* x (+ y z)) (+ (* x y) (* x z))))

   - right distributivity of * over +
   (forall ((x Real) (y Real) (z Real))
    (= (* (+ x y) z) (+ (* x z) (* y z))))

   - non-triviality
   (distinct 0 1)

   - all positive elements have a square root
   (forall (x Real)
    (exists (y Real) (or (= x (* y y)) (= (- x) (* y y)))))

    - axiom schemas for all n > 0
    (forall (x_1 Real) ... (x_n Real)
      (distinct (+ (* x_1 x_1) (+ ... (* x_n x_n)))
         (- 1)))

    - axiom schemas for all odd n > 0 where (^ y n) abbreviates
      the n-fold product of y with itself
    (forall (x_1 Real) ... (x_n Real)
     (exists (y Real)
      (= 0
         (+ (^ y n) (+ (* x_1 (^ y n-1)) (+  ... (+ (* x_{n-1} y) x_n)))))))

    - reflexivity of <=
    (forall (x Real) (<= x x))

    - antisymmetry of <=
    (forall (x Real) (y Real)
      (=> (and (<= x y) (<= y x))
       (= x y)))

    - transitivity of <=
    (forall (x Real) (y Real) (z Real)
     (=> (and (<= x y) (<= y z))
      (<= x z)))

    - totality of <=
    (forall (x Real) (y Real)
      (or (<= x y) (<= y x)))

    - monotonicity of <= wrt +
    (forall (x Real) (y Real) (z Real)
      (=> (<= x y) (<= (+ x z) (+ y z))))

    - monotonicity of <= wrt *
    (forall (x Real) (y Real) (z Real)
      (=> (and (<= x y) (<= 0 z))
       (<= (* z x) (* z y))))

   - definition of <
   (forall (x Real) (y Real)
     (= (< x y) (and (<= x y) (distinct x y))))

  References:
  1) W. Hodges. Model theory. Cambridge University Press, 1993.
  2) PlanetMath, http://planetmath.org/encyclopedia/RealClosedFields.html
 "
)

(theory Reals_Ints

 :smt-lib-version 2.6
 :smt-lib-release "2017-11-24"
 :written_by "Cesare Tinelli"
 :date "2010-04-17"
 :last-updated "2017-11-24"
 :update-history
 "Note: history only accounts for content changes, not release changes.
  2017-11-24 Added abstract values for irrational numbers to set of Real values,
             consistently with the Reals theory (the omission of such values
             was an oversight).
  2015-04-25 Updated to Version 2.5.
 "

 :sorts ((Int 0) (Real 0))

 :funs ((NUMERAL Int)
        (- Int Int)                 ; negation
        (- Int Int Int :left-assoc) ; subtraction
        (+ Int Int Int :left-assoc)
        (* Int Int Int :left-assoc)
        (div Int Int Int :left-assoc)
        (mod Int Int Int)
        (abs Int Int)
        (<= Int Int Bool :chainable)
        (<  Int Int Bool :chainable)
        (>= Int Int Bool :chainable)
        (>  Int Int Bool :chainable)
        (DECIMAL Real)
        (- Real Real)                  ; negation
        (- Real Real Real :left-assoc) ; subtraction
        (+ Real Real Real :left-assoc)
        (* Real Real Real :left-assoc)
        (/ Real Real Real :left-assoc)
        (<= Real Real Bool :chainable)
        (<  Real Real Bool :chainable)
        (>= Real Real Bool :chainable)
        (>  Real Real Bool :chainable)
        (to_real Int Real)
        (to_int Real Int)
        (is_int Real Bool)
       )

 :funs_description
 "All ranked function symbols of the form
    ((_ divisible n) Int Bool)
  where n is a positive numeral.
 "

 :values
 "The set of values for the sort Int consists of
  - all numerals,
  - all terms of the form (- n) where n is a numeral other than 0.

  The set of values for the sort Real consists of
  - an abstract value for each irrational algebraic number
  - all terms of the form (/ (to_real m) (to_real n)) or
    (/ (- (to_real m)) (to_real n)) where
    - m is a numeral,
    - n is a numeral other than 0,
    - as integers, m and n have no common factors besides 1.
 "

 :definition
 "For every expanded signature Sigma, the instance of RealsInts with that
  signature is the theory consisting of all Sigma-models that interpret:

  - the sort Int as the set of all integer numbers,

  - the sort Real as the set of all real numbers,

  - each numeral as the corresponding natural number,

  - to_real as the standard injection of the integers into the reals,

  - the other function symbols with Int arguments as in the theory
    declaration Ints,

  - each decimal as the corresponding real number,

  - to_int as the function that maps each real number r to its integer part,
    that is, to the largest integer n that satisfies (<= (to_real n) r)

  - is_int as the function that maps to true all and only the reals in the
    image of to_real,

  - the other function symbols with Real arguments as in the theory
    declaration Reals.
 "

 :notes
 "By definition of to_int, (to_int (- 1.3)) is equivalent to (- 2), not
   (- 1).
 "

 :notes
 "For each instance T of Reals_Ints, all models of T satisfy the sentence:

  (forall ((x Real))
    (= (is_int x) (= x (to_real (to_int x)))))
 "
)
