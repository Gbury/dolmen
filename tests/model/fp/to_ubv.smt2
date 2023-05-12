(set-logic QF_FPLRA)
(assert (=
  ((_ fp.to_ubv 32) RNE (_ NaN 5 11))
  #b00000000000000000000000000000000))
(check-sat)
