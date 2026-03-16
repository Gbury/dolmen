(set-logic QF_NIA)
(declare-const a Int)
(declare-const b Int)
(assert (= 0 (** a b)))
(check-sat)
(exit)

