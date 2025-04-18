(set-logic ALL)

(declare-const f (-> Int Int))
(declare-const g (-> Int Int Int))
(define-const h (-> Int Int) (lambda ((x Int)) x))
(define-const i (-> Int Int Int) (lambda ((x Int) (y Int)) (+ x y)))

(assert (= 0 (f 0)))
(assert (= 0 (@ f 0)))
(assert (= 0 (g 1 2)))
(assert (= 0 (@ g 1 2)))

(assert (= 0 (@ (g 1) 2)))
(assert (= 0 (i 1 (- 1))))

(check-sat)
