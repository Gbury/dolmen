(set-logic ALL)

(declare-fun a () Int)
(define-fun b () Int (+ a 1))

(assert (not (= a b)))

(check-sat)
