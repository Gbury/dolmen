(set-logic ALL)
(assert (let ((x 1) (x 2)) (= x 0)))
(check-sat)
(exit)
