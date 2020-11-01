(set-logic QF_UF)
(declare-datatype Threevalue ((TVTRUE) (TVFALSE) (TVUNKNOWN) ))
(define-fun tvor ((phi1 Threevalue) (phi2 Threevalue)) Threevalue
	(ite (or (= phi1 TVTRUE) (= phi2 TVTRUE) ) TVTRUE (ite (and (= phi1 TVFALSE) (= phi2 TVFALSE)) TVFALSE TVUNKNOWN ) )
)
