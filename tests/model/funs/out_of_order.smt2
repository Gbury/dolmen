(set-logic ALL)
(declare-fun f (Int) Int)
(assert (= (f 0) 0))
(declare-fun g (Int) Int)
(assert (= (g 0) 1))
(check-sat)
