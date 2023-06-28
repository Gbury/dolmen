(set-logic ALL)
(declare-fun a () Int)
(declare-fun b () Int)

(assert (< a 0))
(check-sat)

(assert (< 0 (* a b)))
(check-sat)

