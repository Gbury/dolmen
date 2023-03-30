(set-logic LIA)
(declare-const x Int)
(assert (= 2 (* x x)))
(check-sat)
