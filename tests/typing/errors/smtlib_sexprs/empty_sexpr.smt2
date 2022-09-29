(set-logic ALL)

(assert (forall ((x Int)) (! (= x x) :pattern (()))))
