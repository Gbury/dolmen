(set-logic QF_FPLRA)
(assert (let (
  (a (_ +zero 2 4))
  (b (_ -zero 2 4))
  ) (not (= a b))))
(check-sat)
