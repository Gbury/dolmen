(set-option :print-success false)
(set-option :produce-models true)
(set-logic QF_BVFPLRA)
(declare-fun x9 () (_ BitVec 32))
(declare-fun x1 () (_ FloatingPoint 11 53))
(declare-fun x7 () (_ FloatingPoint 11 53))
(declare-fun x4 () RoundingMode)
(declare-fun x2 () (_ FloatingPoint 11 53))
(declare-fun x3 () (_ FloatingPoint 11 53))
(declare-fun x5 () (_ BitVec 32))
(declare-fun x8 () (_ BitVec 32))
(declare-fun x6 () (_ FloatingPoint 11 53))
(assert (and (bvsle (_ bv1072010280 32) x8) (= ((_ to_fp 11 53) x4 x5) x7) (= (fp.mul x4 (fp.sub x4 x7 (fp.mul x4 ((_ to_fp 11 53) x4 2.0) (fp.sub x4 x2 (fp.sub x4 (fp.div x4 (fp.mul x4 x6 x6) (fp.add x4 x7 x6)) x3)))) ((_ to_fp 11 53) x4 (bvadd (_ bv1 32) (bvneg (bvand (_ bv2 32) (bvashr x9 (_ bv30 32))))))) x1)))
(check-sat)
(get-model)
(exit)