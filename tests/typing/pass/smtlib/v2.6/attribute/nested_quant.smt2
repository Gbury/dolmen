(set-logic ALL)
(assert
  (forall ((x Int))
    (forall ((y Int))
          (= x y)
    )
  )
)
