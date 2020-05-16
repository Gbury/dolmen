(set-logic QF_UFLIADT)
(declare-datatypes ((list 1)) ((par (alpha) ((nil) (cons (head (list alpha)))))))
(assert (= (as nil (list Int)) (nil true)))
(check-sat)
