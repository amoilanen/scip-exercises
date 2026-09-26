(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; Without the explicit delay, the recursive call is an argument of
;; interleave and is evaluated at once, so flattening walks the entire
;; stream of streams before producing anything.  For an infinite stream of
;; frames, as produced by a recursive rule, that never finishes.

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (louis-flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave (stream-car stream)
                  (louis-flatten-stream (stream-cdr stream)))))

(initialize-data-base!
 (append microshaft-data-base
         '((married Minnie Mickey)
           (rule (married ?x ?y)
                 (married ?y ?x)))))

(define query '(and (married Mickey ?who) (married ?who ?x)))

(check (run-query-head query 1)
       => '((and (married Mickey Minnie) (married Minnie Mickey))))

(set! flatten-stream louis-flatten-stream)

(check (run-query-bounded query 50) => '(diverged))
(check (run-query '(supervisor ?x (Scrooge Eben)))
       => '((supervisor (Cratchet Robert) (Scrooge Eben))))
