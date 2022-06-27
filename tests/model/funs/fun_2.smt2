(set-logic QF_UF)

(declare-sort t 0)

(declare-fun a () t)
(declare-fun b () t)
(declare-fun c () t)
(declare-fun f (t) t)

(assert (= a (f a)))
(assert (= b (f b)))
(assert (= b (f c)))

(check-sat)
