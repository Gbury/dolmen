(set-logic ALL)

(declare-fun a1 () Float64)
(declare-fun b1 () Float64)
(assert (fp.isZero a1))
(assert (fp.isPositive a1))
(assert (= (fp.roundToIntegral RTP a1) b1))

(declare-fun a2 () Float64)
(declare-fun b2 () Float64)
(assert (fp.isZero a2))
(assert (fp.isNegative a2))
(assert (= (fp.roundToIntegral RTP a2) b2))

(declare-fun a3 () Float64)
(declare-fun b3 () Float64)
(declare-fun m3 () RoundingMode)
(assert (fp.isNegative a3))
(assert (fp.isNegative b3))
(assert (= (fp.roundToIntegral m3 a3) b3))

(check-sat)
