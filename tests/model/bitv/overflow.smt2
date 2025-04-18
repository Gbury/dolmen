(set-logic QF_BV)
(define-fun umaxint () (_ BitVec 3) #b111)
(define-fun smaxint () (_ BitVec 3) #b011)
(define-fun sminint () (_ BitVec 3) #b100)
(define-fun zero () (_ BitVec 3) #b000)
(define-fun one () (_ BitVec 3) #b001)

; testing bvnego
(assert (bvnego sminint))
(assert (not (bvnego zero)))
(assert (not (bvnego one)))
(assert (not (bvnego smaxint)))

; Testing bvuaddo
(assert (bvuaddo umaxint one))
(assert (not (bvuaddo one one)))

; Testing bvsaddo
(assert (bvsaddo smaxint one))
(assert (not (bvsaddo one one)))
(assert (not (bvsaddo sminint one)))

; Testing bvusubo
(assert (bvusubo zero one))
(assert (not (bvusubo one one)))
(assert (not (bvusubo umaxint one)))

; Testing bvssubo
(assert (bvssubo sminint one))
(assert (not (bvssubo one one)))
(assert (not (bvssubo zero one)))
(assert (not (bvssubo smaxint one)))

(check-sat)
