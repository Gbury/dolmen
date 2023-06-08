(set-logic QF_UF)

(declare-sort t 0)

(declare-fun a () t)
(declare-fun b () t)
(declare-fun f (t) t)

(assert (not (= a (f b))))

(check-sat)
