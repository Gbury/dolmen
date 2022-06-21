(set-logic ALL)

(declare-datatypes ((list 1)) ((par (alpha) (
  (nil)
  (cons (head alpha) (tail (list alpha)))
))))

(define-fun-rec length ((l (list Int))) Int
  (ite
    ((_ is nil) l)
    0
    (+ 1 (length (tail l)))
  )
)

(declare-fun l () (list Int))

(assert (= (length l) 2))

(check-sat)
