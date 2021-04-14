(set-logic QF_LIA)
(declare-fun a () Int)
(assert (let ((a 0)) (= a a)))
(check-sat)
