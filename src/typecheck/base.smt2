(theory Core

 :smt-lib-version 2.6
 :smt-lib-release "2017-11-24"
 :written-by "Cesare Tinelli"
 :date "2010-04-17"
 :last-updated "2015-04-25"
 :update-history
 "Note: history only accounts for content changes, not release changes.
  2015-04-25 Updated to Version 2.5.
  2010-08-15 Minor fix.
 "

 :sorts ((Bool 0))

 :funs ((true Bool)
        (false Bool)
        (not Bool Bool)
        (=> Bool Bool Bool :right-assoc)
        (and Bool Bool Bool :left-assoc)
        (or Bool Bool Bool :left-assoc)
        (xor Bool Bool Bool :left-assoc)
        (par (A) (= A A Bool :chainable))
        (par (A) (distinct A A Bool :pairwise))
        (par (A) (ite Bool A A A))
       )

 :definition
 "For every expanded signature Sigma, the instance of Core with that signature
  is the theory consisting of all Sigma-models in which:

  - the sort Bool denotes the set {true, false} of Boolean values;

  - for all sorts s in Sigma,
    - (= s s Bool) denotes the function that
      returns true iff its two arguments are identical;
    - (distinct s s Bool) denotes the function that
      returns true iff its two arguments are not identical;
    - (ite Bool s s) denotes the function that
      returns its second argument or its third depending on whether
      its first argument is true or not;

  - the other function symbols of Core denote the standard Boolean operators
    as expected.
 "
 :values
 "The set of values for the sort Bool is {true, false}."
)
