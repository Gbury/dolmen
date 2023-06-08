(set-logic QF_FPLRA)
(assert (let (
  (a (_ +zero 5 11))
  (b (_ -zero 5 11))
  ) (= (fp.min a b) a)))
(check-sat)
