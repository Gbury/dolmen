(set-logic BVLIA)
(declare-const b (_ BitVec 5))
(assert (= b ((_ int2bv 5) 13) ))
(check-sat)
(exit)
