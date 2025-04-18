(set-logic ALL)
(declare-datatypes ((t 0)) (((A) (B) (C) (D))))
(define-fun f ((x t)) t (match x _))
(check-sat)
