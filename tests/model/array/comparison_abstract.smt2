(set-logic ALL)

(declare-fun a () (Array Int Int))
(declare-fun b () (Array Int Int))
(declare-fun c () (Array Int Int))
(assert (= a b))
(assert (not (= a c)))
(check-sat)
