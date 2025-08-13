(set-logic ALL)
(define-fun-rec f ((x Int) (x Int)) Int (ite (= x 0) 0 (f (- x 1) (- x 1))))
(assert (= (f 0 1) 0))
(check-sat)
