(set-logic QF_UFDTLIA)
(declare-datatypes ((list 1)) ((par (alpha) ((nil) (cons (head (list alpha)))))))
(assert (= (as nil (list Int)) (nil true)))
(check-sat)
