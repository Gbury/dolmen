(set-logic ALL)
(declare-datatypes ((t 0)) (((A) (B) (C) (D))))
(define-fun f ((x t)) t (match x ((A B) (lambda x))))
(check-sat)
