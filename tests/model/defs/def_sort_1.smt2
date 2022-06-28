(set-logic ALL)

(define-sort t () Int)

(declare-fun a () t)
(declare-fun b () Int)

(assert (= a b))

(check-sat)
