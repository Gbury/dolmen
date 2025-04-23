(set-logic ALL)
(declare-sort-parameter A)
(declare-const f (-> A A))
(define-const id (-> A A) (lambda ((x A)) x))

(assert (forall ((x A)) (= x (f x))))

(assert (= (f 0) (id 0)))

(assert (forall ((x Int)) (= (f x) (id x))))

(assert (forall ((x A)) (= (f x) (id x))))

(declare-sort-parameter B)
(assert (forall ((x B)) (= (f x) (id x))))

(check-sat)

