(set-logic QF_SLIA)

; Positions and lengths are about code points, not UTF-8 bytes.
(declare-fun s () String)
(assert (= s "a\u{e9}\u{1f600}z"))

(assert (= (str.len s) 4))
(assert (= (str.at s 2) "\u{1f600}"))
(assert (= (str.substr s 1 2) "\u{e9}\u{1f600}"))
(assert (= (str.indexof s "\u{1f600}" 0) 2))
(assert (= (str.to_code "\u{1f600}") 128512))
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))

(check-sat)
