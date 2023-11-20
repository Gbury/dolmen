(set-logic UFNIA)
(declare-const x Int)
(declare-const y Int)
(assert
  (forall ((z Int))
    (= (! (+ x y z) :named t ) t)
  )
)
(exit)
