(set-logic QF_UF)

(declare-sort s 0)
(declare-sort t 0)

(declare-fun a () s)
(declare-fun b () t)
(declare-fun f (s) t)

(assert (not (= b (f a))))

(check-sat)
