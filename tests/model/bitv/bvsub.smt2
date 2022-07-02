(set-logic QF_BV)
(assert (= (bvneg (_ bv1 32)) (bvsub (_ bv10 32) (_ bv11 32))))
(check-sat)
