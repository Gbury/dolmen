(set-logic ALL)
(declare-datatypes ((foo 0)) ((
  (A)
  (B)
  (C)
)))

(define-fun ok_1 ((x foo)) Int
  (match x (
    (A 0)
    (_ 1)
    )
  )
)

(define-fun ok_2 ((x foo)) Int
  (match x (
    (C 0)
    (_ 1)
    )
  )
)

(define-fun redundant_1 ((x foo)) Int
  (match x (
    (B 0)
    (_ 1)
    (A 2)
    )
  )
)

(define-fun redundant_2 ((x foo)) Int
  (match x (
    (B 0)
    (_ 1)
    (_ 2)
    )
  )
)
