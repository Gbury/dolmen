(set-logic ALL)

(declare-const a String)
(declare-const b String)
(assert (= (str.++ a b) "abcd"))
(assert (= (str.++ b a) "cdab"))
(assert (not (= (str.len a) (str.len b) 2)))
(check-sat)
