(set-logic ALL)

(define-sort t () Int)

(declare-fun a () t)
(define-fun b () t a)

(assert (= a b))

(check-sat)
