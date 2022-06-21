(set-logic ALL)

(declare-datatypes ((list 1)) ((par (alpha) (
  (nil)
  (cons (head alpha) (tail (list alpha)))
))))

(define-fun-rec length ((l (list Int))) Int
  (match l (
    (nil 0)
    ((cons h tl) (+ 1 (length tl)))
    )
  )
)

(declare-fun l () (list Int))

(assert (= (length l) 2))

(check-sat)
