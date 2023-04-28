(set-logic ALL)

(declare-datatypes ((list 1)) ((par (alpha) (
  (nil)
  (cons (head alpha) (tail (list alpha)))
))))

(assert (= 0 (head (tail (as nil (list Int))))))
(check-sat)
