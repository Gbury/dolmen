(set-logic ALL)
(define-fun f ((x Int) (x Int)) Bool (= x x))
(check-sat)
(exit)
