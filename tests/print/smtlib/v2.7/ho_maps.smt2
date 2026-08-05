(set-logic HO_LIA)

(declare-sort-parameter A)
(declare-const id_ (-> A A))

; Check whether the printing/export funcitonality correctly inserts
; the required type annotation on the inner `id_`
(define-const b1 Int (id_ (as id_ (-> Int Int)) 5))

(assert (
  forall ((x Int)) (
    ! (= x x)
    :pattern ((id_ (as id_ (-> Int Int)) x))
  )
 )
)
