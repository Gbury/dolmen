(set-logic ALL)

(declare-sort t 0)

(declare-fun a () t)
(define-fun f () t a)

(assert (= a f))

(check-sat)
