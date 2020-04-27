(set-logic QF_UFDT)
(declare-datatype Threevalue ((TVTRUE) (TVFALSE) (TVUNKNOWN)))
(define-fun tvnot ((phi Threevalue)) Threevalue
	(ite (= phi TVTRUE) TVFALSE (ite (= phi TVFALSE) TVTRUE TVUNKNOWN) )
)
