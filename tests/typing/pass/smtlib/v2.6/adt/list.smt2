(set-logic QF_UFDTLIA)
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
(assert (= 0 (length (as nil (list Int)))))
(define-fun-rec is_nil ((l (list Int))) Bool
  ((_ is nil) l)
)
