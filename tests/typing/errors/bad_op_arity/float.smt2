(set-logic FP)
(declare-fun x () Float16)
(assert (= x ((_ to_fp 3 4) x x x)))
