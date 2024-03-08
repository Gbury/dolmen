(set-logic ALL)
(assert
  (let ((x 5))
    (! (= x x)
     :foo bar
    )
  )
)
