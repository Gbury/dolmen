(set-logic ALL)

(declare-fun a1 () Float64)
(assert (fp.isNaN a1))

(declare-fun a2 () Float64)
(assert (fp.isZero a2))

(declare-fun a3 () Float64)
(assert (fp.isSubnormal a3))

(declare-fun a4 () Float64)
(assert (fp.isInfinite a4))

(declare-fun a5 () Float64)
(declare-fun a6 () Float64)
(declare-fun a7 () Float64)
(assert (= (fp.add RNE a5 a6) a7))

(declare-fun a8 () Float64)
(declare-fun a9 () Float64)
(declare-fun a10 () Float64)
(declare-fun a11 () Float64)
(assert (= (fp.mul RTP a8 a9) (fp.mul RTN a10 a11)))

(assert (distinct a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
(check-sat)
