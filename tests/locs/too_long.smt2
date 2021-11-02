(set-logic LIA)
(assert (and true false true false true false true false true false true false true false true false true false true false true false true false (= 0 (* 1 2))))
(check-sat)
