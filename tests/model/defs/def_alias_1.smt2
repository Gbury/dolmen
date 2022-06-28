(set-logic ALL)

(declare-fun a () Int)
(define-fun b () Int a)

(assert (= a b))

(check-sat)
