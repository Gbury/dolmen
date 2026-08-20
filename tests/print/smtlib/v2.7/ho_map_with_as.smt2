  (set-logic HO_UFLIA)

(declare-sort-parameter A)
(declare-sort-parameter B)
(declare-const f (-> A B))

(assert (forall ((x Int)) (= (@ (as f (-> Int Int)) x) x)))
