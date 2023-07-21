(set-logic BV)
(declare-const x (_ BitVec 11))
(assert (= x ((_ extract 10 0) #b1010)))
(check-sat)
