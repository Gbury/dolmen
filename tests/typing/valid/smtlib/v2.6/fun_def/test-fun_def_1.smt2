(set-logic QF_UF)
(declare-datatype Threevalue ((TVTRUE) (TVFALSE) (TVUNKNOWN)))
(define-fun tvnot ((phi Threevalue)) Threevalue
	(ite (= phi TVTRUE) TVFALSE (ite (= phi TVFALSE) TVTRUE TVUNKNOWN) )
)
