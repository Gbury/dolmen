(theory ArraysEx

 :smt-lib-version 2.6
 :smt-lib-release "2017-11-24"
 :written-by "Cesare Tinelli"
 :date "2010-04-28"
 :last-updated "2015-04-25"
 :update-history
 "Note: history only accounts for content changes, not release changes.
  2015-04-25 Updated to Version 2.5.
  2010-08-15 Minor fix.
 "

 :sorts ((Array 2))

 :funs ((par (X Y) (select (Array X Y) X Y))
        (par (X Y) (store (Array X Y) X Y (Array X Y))) )

 :notes "A schematic version of the theory of functional arrays with extensionality."

 :definition
 "For every expanded signature Sigma, the instance of ArraysEx with that signature
  is the theory consisting of all Sigma-models that satisfy all axioms of the form
  below, for all sorts s1, s2 in Sigma:

  - (forall ((a (Array s1 s2)) (i s1) (e s2))
      (= (select (store a i e) i) e))

  - (forall ((a (Array s1 s2)) (i s1) (j s1) (e s2))
      (=> (distinct i j)
               (= (select (store a i e) j) (select a j))))

  - (forall ((a (Array s1 s2)) (b (Array s1 s2)))
      (=> (forall ((i s1)) (= (select a i) (select b i)))
               (= a b)))
 "

 :values
 "For all sorts s1, s2 in in the signature, the values of sort (Array s1 s2) are
  abstract.
 "
)


