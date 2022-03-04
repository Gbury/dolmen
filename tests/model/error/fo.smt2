(set-logic ALL)
(declare-fun p (Int) Bool)
(assert (forall ((x Int)) (p x)))
(check-sat)
