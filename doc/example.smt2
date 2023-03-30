(set-logic LIA)
(declare-const x Int)
(assert (= 2 (+ 1 x)))
(check-sat)
