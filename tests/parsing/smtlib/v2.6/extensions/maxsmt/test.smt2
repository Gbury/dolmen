(set-logic ALL)
(declare-fun a () Int)
(declare-fun b () Int)
(minimize a)
(maximize b)
(assert (< b a))
(check-sat)