(set-logic ALL)
(declare-fun a () Real)
(declare-fun z () Real)
(declare-fun c () Real)
(assert (= z 0.0))
(assert (= c (/ a z)))
(check-sat)