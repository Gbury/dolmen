(set-logic ALL)

(declare-sort-parameter A)

(declare-const id_ (-> A A))
(declare-const add_ (-> Int Int Int))

(define-const id (-> A A) (lambda ((x A)) x))
(define-const add (-> Int Int Int) (lambda ((x Int) (y Int)) (+ x y)))

(assert (= 0 (id 0)))
(assert (= 0 (id_ 0)))

(assert (= 0 (@ id 0)))
(assert (= 0 (@ id_ 0)))

(assert (= (id 1) (id_ 1)))
(assert (= (@ id 1) (id_ 1)))
(assert (= (id 1) (@ id_ 1)))
(assert (= (@ id 1) (@ id_ 1)))

(assert (= (id 13.0) (id_ 13.0)))
(assert (= (@ id 13.0) (id_ 13.0)))
(assert (= (id 13.0) (@ id_ 13.0)))
(assert (= (@ id 13.0) (@ id_ 13.0)))

(assert (= 3 (add 1 2)))
(assert (= 3 (@ add 1 2)))

(assert (= 3 (add_ 1 2)))
(assert (= 3 (@ add_ 1 2)))

(check-sat)
