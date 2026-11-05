(set-logic QF_SLIA)

; Regex membership and regex-driven replacement (values cross-checked with cvc5).
(declare-fun s () String)
(assert (= s "hello"))

; str.replace_re / replace_re_all use the first shortest non-empty match.
(assert (= (str.replace_re s (re.+ (re.range "a" "z")) "_") "_ello"))
(assert (= (str.replace_re "a12b34" (re.+ (re.range "0" "9")) "#") "a#2b34"))
(assert (= (str.replace_re_all "a12b34" (re.+ (re.range "0" "9")) "#") "a##b##"))

; inclusion (nested star, intersection, complement, and repetition)
(assert (str.in_re "abab" (re.* (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (str.in_re "ab" (re.inter (re.* re.allchar) (re.comp (str.to_re "ba")))))
(assert (str.in_re "aaa" ((_ re.loop 2 4) (str.to_re "a"))))
(assert (not (str.in_re "a" ((_ re.loop 2 4) (str.to_re "a")))))
(assert (str.in_re s (re.++ (str.to_re "he") (re.+ (re.range "a" "z")))))

(check-sat)
