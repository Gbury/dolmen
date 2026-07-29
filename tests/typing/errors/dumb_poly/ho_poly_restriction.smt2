(set-logic ALL)

(declare-sort Pair 2)

(declare-sort-parameter A)
(declare-sort-parameter B)
(declare-const pair (-> A B (Pair A B)))

; These are fine because SMT-LIB lies/cheats with
; the supposed associativity of '@' and treat these
; as one application with n arguments (rather than n
; binary applications)
(define-const a1 (Pair Int Int) (pair 13 42))
(define-const a2 (Pair Int Int) (@ pair 13 42))

; This is also fine because the type annotation fixes all type arguments
; to the `pair` application
(define-const b1 (Pair Int Int) (@ ((as pair (-> Int (Pair Int Int))) 13) 42))

; Should raise an error, because the intermediate map has a free (weak) type variable
(define-const b2 (Pair Int Int) (@ (pair 13) 42))

(check-sat)
(exit)
