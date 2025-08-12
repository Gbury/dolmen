(set-logic QF_BV)
; check that innocuous shadowing between a parameter and non-parameter
; are treated as warnings and not an error (overlapping between parameters
; of the same definitions are dangerous in SMT-LIB)
(declare-const x Bool)
(define-fun P ((x Bool)) Bool x)
; also check that we don't hack and only use line numbers to trigger the error
(declare-const y Bool)(define-fun Q ((y Bool)) Bool y)
(assert (and (P false) (Q true)))
(check-sat)
