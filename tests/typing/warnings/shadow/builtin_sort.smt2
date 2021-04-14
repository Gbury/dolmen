(set-logic QF_DTLIA)
(declare-datatypes ((list 1)) (
  (par (Int) (
    (nil)
    (cons
      (head Int)
      (tail (list Int))
    ))
  )))
