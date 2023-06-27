(set-logic NRA)
(declare-fun a () Real)
; the assert does not matter
(assert (= a 1))
(check-sat)
