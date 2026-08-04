(set-logic ALL)

(declare-sort Pair 2)

(declare-sort-parameter A)
(declare-sort-parameter B)
(declare-const pair (-> A B (Pair A B)))

(declare-sort-parameter C)
(declare-const id_ (-> C C))

(declare-sort-parameter D)
(declare-fun id (D) D)

; Some simple test for polymorphism instantiation
(define-const a1 Int (id 5))
(define-const a2 Int (id_ 5))
(define-const a3 Int (@ id_ 5))

; These should raise a warning/error, because the intermediate
; `id_`'s application needs a polymorphism instantiation with type arguments
; that cannot be locally determined (since it depends on the top-level application)
(define-const b1 Int (id_ id_ 5))
(define-const b2 Int (@ id_ id_ 5))

; Just some tests regarding associativity
(define-const c1 Int (@ (id_ id_) 5))
(define-const c2 Int (id_ (id_ 5)))

; These are fine because SMT-LIB lies/cheats with
; the supposed associativity of '@' and treat these
; as one application with n arguments (rather than n
; binary applications)
(define-const p1 (Pair Int Int) (pair 13 42))
(define-const p2 (Pair Int Int) (@ pair 13 42))

; This is also fine because the type annotation fixes all type arguments
; to the `pair` application
(define-const p3 (Pair Int Int) (@ ((as pair (-> Int (Pair Int Int))) 13) 42))

; Should raise a warning/error, because the intermediate map has a free (weak) type variable
(define-const p4 (Pair Int Int) (@ (pair 13) 42))

(check-sat)
(exit)
