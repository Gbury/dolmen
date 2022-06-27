(set-logic ALL)

(declare-sort t 0)

(declare-fun a () t)
(declare-fun b () t)
(define-fun f ((x t)) t x)

(assert (= a (f b)))

(check-sat)
