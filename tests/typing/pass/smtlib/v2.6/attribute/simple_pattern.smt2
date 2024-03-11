; This tests that attributes are correctly taken into account
; To do that, we rely on the unknown attribute warning
(set-logic ALL)
(assert
  (forall ((x Int))
    (! (= x x)
     :foo bar
    )
  )
)
