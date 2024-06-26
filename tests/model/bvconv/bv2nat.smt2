(set-logic BVLIA)
(declare-const b (_ BitVec 5))
(assert (= 5 (bv2nat b)))
(check-sat)
(get-model)
(exit)

