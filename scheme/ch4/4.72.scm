(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; A stream being combined may be infinite.  Appending would then never get
;; past the first stream, hiding the answers of the other disjuncts or of
;; the other frames forever; interleaving takes elements from all of them.

(define (appending-disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (stream-append-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (appending-disjoin (rest-disjuncts disjuncts) frame-stream)))))

(define (appending-flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (stream-append-delayed
       (stream-car stream)
       (delay (appending-flatten-stream (stream-cdr stream))))))

(initialize-data-base!
 (append microshaft-data-base
         '((married Minnie Mickey)
           (rule (married ?x ?y)
                 (married ?y ?x))
           (person Mickey)
           (person Minnie))))

(define disjunction '(or (married Mickey ?x) (supervisor ?x (Bitdiddle Ben))))
(define conjunction '(and (person ?p) (married ?p ?q)))

(define (second-clause answer) (caddr answer))

(check (map second-clause (run-query-head disjunction 4))
       => '((supervisor Minnie (Bitdiddle Ben))
            (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
            (supervisor Minnie (Bitdiddle Ben))
            (supervisor (Fect Cy D) (Bitdiddle Ben))))

(check (run-query-head conjunction 2)
       => '((and (person Mickey) (married Mickey Minnie))
            (and (person Minnie) (married Minnie Mickey))))

(put 'or 'qeval appending-disjoin)
(set! flatten-stream appending-flatten-stream)

(check (map second-clause (run-query-head disjunction 4))
       => '((supervisor Minnie (Bitdiddle Ben))
            (supervisor Minnie (Bitdiddle Ben))
            (supervisor Minnie (Bitdiddle Ben))
            (supervisor Minnie (Bitdiddle Ben))))

(check (run-query-head conjunction 2)
       => '((and (person Mickey) (married Mickey Minnie))
            (and (person Mickey) (married Mickey Minnie))))
