(set-logic QF_UF)
(declare-fun a () Bool)
(push 1)
  (declare-fun b () Bool)
  (assert (or a b))
  (check-sat)
(pop 1)
(assert (or a b))
(check-sat)
