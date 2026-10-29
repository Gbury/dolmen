(set-logic QF_SLIA)

; The model claims s is in (ab)+, but "aba" is not.
; The model verification must reject it.
(declare-fun s () String)
(assert (str.in_re s (re.+ (re.++ (str.to_re "a") (str.to_re "b")))))

(check-sat)
