(set-logic BV)
(declare-fun b () (_ BitVec 5))
(assert (let
  ((a ((_ extract 2 3) b)))
  (= a a)
))
