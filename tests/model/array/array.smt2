(set-logic ALL)

(declare-sort t 0)
(declare-fun x () t)
(declare-fun y () t)

(declare-fun a () (Array t t))
(assert (= (select a x) x))
(assert (= (select a y) y))
(check-sat)
