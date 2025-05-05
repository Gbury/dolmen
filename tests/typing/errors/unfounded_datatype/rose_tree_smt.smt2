(set-logic ALL)

(declare-datatypes ((list 1)) (
  (par (alpha) (
    (nil)
    (cons (head alpha) (tail (list alpha)))
  ))
))

(declare-datatypes ((tree 1)) (
  (par (alpha) (
    (node (elt alpha) (children (list (tree alpha))))
  ))
))
