(set-logic ALL)

(declare-datatypes ((int_ref 0))
(((mk_int_ref (int_content Int)))))

(define-fun int_ref_projection ((a int_ref)) Int (int_content a))

(check-sat)
