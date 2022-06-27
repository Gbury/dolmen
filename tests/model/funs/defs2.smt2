(set-logic ALL)

(declare-sort t 0)

(declare-fun a () t)
(define-fun b () t a)

(assert (= a b))

(check-sat)
