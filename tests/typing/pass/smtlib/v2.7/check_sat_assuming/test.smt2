(set-logic ALL)
(declare-const a Bool)
(declare-const b Bool)
(check-sat-assuming ((and a b) (or a b)))
