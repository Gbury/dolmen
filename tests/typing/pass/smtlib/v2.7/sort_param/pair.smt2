(set-logic ALL)
(declare-sort Pair 2)
(declare-sort-parameter A)
(declare-sort-parameter B)
(declare-fun pair (A B) (Pair A B))
(declare-fun fst ((Pair A B)) A)
(declare-fun snd ((Pair A B)) B)

(assert (forall ((x A) (y B)) (= x (fst (pair x y)))))

(assert (forall ((x A) (y B)) (= y (snd (pair x y)))))

(assert (forall ((p (Pair A B)))
  (= p (pair (fst p) (snd p)))
))

(check-sat)
