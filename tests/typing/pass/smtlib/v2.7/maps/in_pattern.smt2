(set-option :print-success false)
(set-logic ALL)
(define-sort |Z| () Int)
(declare-sort P 1)
(define-sort |POW Z| () (P |Z|))
(declare-fun |set.in Z| (|Z| |POW Z|) Bool)
(declare-const |set.empty Z| |POW Z|)
(assert (!
  (forall ((e |Z|)) (!
    (not (|set.in Z| e |set.empty Z|))
    :pattern ((|set.in Z| e |set.empty Z|))))
  :named |ax.set.in.empty Z|))
;; set Z
(define-sort |? Z| () (-> |Z| Bool))
(declare-const |set.intent Z| (-> |? Z| |POW Z|))
(assert (!
  (forall ((p |? Z|) (x |Z|)) (!
    (= (|set.in Z| x (|set.intent Z| p))
       (@ p x))
    :pattern ((|set.in Z| x (|set.intent Z| p)))))
  :named |ax.rw.intent Z|))
(exit)
