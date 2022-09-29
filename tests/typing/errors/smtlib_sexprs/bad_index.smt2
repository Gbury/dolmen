(set-logic ALL)

(assert (forall ((x Int)) (! (= x x) :pattern ((_ foo 1 a 3)))))
