(set-logic QF_SLIA)

; SMT-LIB totalization of the theory's partial functions.
(assert (= (str.at "abc" 5) ""))             ; index past end
(assert (= (str.at "abc" (- 1)) ""))         ; negative index
(assert (= (str.substr "abc" 1 10) "bc"))    ; length clamped to what remains
(assert (= (str.substr "abc" (- 1) 2) ""))   ; negative start
(assert (= (str.substr "abc" 3 1) ""))       ; start = len
(assert (= (str.indexof "abc" "" 2) 2))      ; empty needle matches at i
(assert (= (str.indexof "abc" "" 3) 3))      ; ... including i = len
(assert (= (str.indexof "abc" "b" 5) (- 1))) ; i > len
(assert (= (str.to_int "") (- 1)))           ; empty is not a numeral
(assert (= (str.to_int "007") 7))            ; leading zeros allowed
(assert (= (str.from_int (- 3)) ""))         ; negative has no numeral
(assert (= (str.to_code "") (- 1)))          ; not a singleton
(assert (= (str.to_code "ab") (- 1)))        ; not a singleton
(assert (= (str.from_code (- 1)) ""))        ; invalid code point
(assert (= (str.from_code 200000) ""))       ; above 0x2FFFF
(assert (= (str.replace "abc" "" "X") "Xabc"))   ; empty pattern prepends
(assert (= (str.replace_all "abc" "" "X") "abc")); empty pattern is a no-op

(check-sat)
