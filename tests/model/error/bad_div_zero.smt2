(set-logic ALL)
(declare-fun a () Int)
(declare-fun b () Int)
(declare-fun c () Int)
(declare-fun d () Int)
(declare-fun z () Int)
(assert (= z 0))
(assert (= c (div a z)))
(assert (= d (div b z)))
(assert (not (= c d)))
(check-sat)
