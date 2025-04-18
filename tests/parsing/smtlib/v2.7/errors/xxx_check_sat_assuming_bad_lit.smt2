(set-logic QF_LRA)
(set-info :status unsat)
(declare-fun a () Real)
(declare-fun b () Real)

(assert (<= a b))
(check-sat-assuming ( (>= a 1) (<= b (/ 1 2)) ))
(exit)
