(set-logic QF_UF)
(assert (and (= a b) (= b c) (not (= a c))))
(check-sat)
