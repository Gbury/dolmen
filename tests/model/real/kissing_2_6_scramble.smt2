(set-option :print-success false)
(set-option :produce-models true)
(set-logic QF_NRA)
(declare-fun x6 () Real)
(declare-fun x5 () Real)
(declare-fun x10 () Real)
(declare-fun x11 () Real)
(declare-fun x7 () Real)
(declare-fun x1 () Real)
(declare-fun x12 () Real)
(declare-fun x2 () Real)
(declare-fun x4 () Real)
(declare-fun x8 () Real)
(declare-fun x3 () Real)
(declare-fun x9 () Real)
(assert (= (+ (* x1 x1) (* x11 x11)) 1))
(assert (>= (+ (* (- x10 x8) (- x10 x8)) (* (- x12 x4) (- x12 x4))) 1))
(assert (= (+ (* x2 x2) (* x6 x6)) 1))
(assert (>= (+ (* (- x1 x5) (- x1 x5)) (* (- x11 x7) (- x11 x7))) 1))
(assert (<= 1 (+ (* (- x2 x5) (- x2 x5)) (* (- x6 x7) (- x6 x7)))))
(assert (>= (+ (* (- x12 x11) (- x12 x11)) (* (- x10 x1) (- x10 x1))) 1))
(assert (>= (+ (* (- x8 x5) (- x8 x5)) (* (- x4 x7) (- x4 x7))) 1))
(assert (= (+ (* x4 x4) (* x8 x8)) 1))
(assert (<= 1 (+ (* (- x2 x1) (- x2 x1)) (* (- x6 x11) (- x6 x11)))))
(assert (>= (+ (* (- x10 x2) (- x10 x2)) (* (- x12 x6) (- x12 x6))) 1))
(assert (= 1 (+ (* x12 x12) (* x10 x10))))
(assert (<= 1 (+ (* (- x9 x8) (- x9 x8)) (* (- x3 x4) (- x3 x4)))))
(assert (<= 1 (+ (* (- x10 x5) (- x10 x5)) (* (- x12 x7) (- x12 x7)))))
(assert (>= (+ (* (- x9 x1) (- x9 x1)) (* (- x3 x11) (- x3 x11))) 1))
(assert (= (+ (* x9 x9) (* x3 x3)) 1))
(assert (>= (+ (* (- x9 x5) (- x9 x5)) (* (- x3 x7) (- x3 x7))) 1))
(assert (<= 1 (+ (* (- x9 x2) (- x9 x2)) (* (- x3 x6) (- x3 x6)))))
(assert (= (+ (* x7 x7) (* x5 x5)) 1))
(assert (>= (+ (* (- x8 x1) (- x8 x1)) (* (- x4 x11) (- x4 x11))) 1))
(assert (>= (+ (* (- x3 x12) (- x3 x12)) (* (- x9 x10) (- x9 x10))) 1))
(assert (>= (+ (* (- x2 x8) (- x2 x8)) (* (- x6 x4) (- x6 x4))) 1))
(check-sat)
(get-model)
(exit)
