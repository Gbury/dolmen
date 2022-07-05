(set-logic ALL)

(declare-fun a1 () Float64)
(declare-fun b1 () Float64)
(declare-fun c1 () Float64)
(assert (fp.isInfinite a1))
(assert (= (fp.rem a1 b1) c1))

(declare-fun a2 () Float64)
(declare-fun b2 () Float64)
(declare-fun c2 () Float64)
(assert (fp.isNaN a2))
(assert (= (fp.rem a2 b2) c2))

(declare-fun a3 () Float64)
(declare-fun b3 () Float64)
(declare-fun c3 () Float64)
(assert (= (fp.rem a3 b3) c3))

(declare-fun a4 () Float64)
(declare-fun b4 () Float64)
(declare-fun c4 () Float64)
(assert (= (fp.rem a4 b4) c4))

(declare-fun a5 () Float64)
(declare-fun b5 () Float64)
(declare-fun c5 () Float64)
(assert (= (fp.rem a5 b5) c5))

(assert (distinct a3 b3 c3 a4 b4 c4 a5 b5 c5))

(check-sat)
