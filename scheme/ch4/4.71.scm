(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; Louis's versions evaluate the rule applications and the remaining
;; disjuncts before returning anything.  When those computations loop, the
;; delayed versions still produce the answers found so far (the stream is
;; infinite or the loop comes later), while Louis's never return at all.

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (louis-simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (louis-disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave (qeval (first-disjunct disjuncts) frame-stream)
                  (louis-disjoin (rest-disjuncts disjuncts) frame-stream))))

(initialize-data-base!
 (append microshaft-data-base
         '((married Minnie Mickey)
           (rule (married ?x ?y)
                 (married ?y ?x))
           (rule (loop ?x)
                 (loop ?x)))))

(define married-query '(married Mickey ?who))
(define looping-disjunction '(or (supervisor ?x (Bitdiddle Ben)) (loop ?x)))

(check (run-query-head married-query 2)
       => '((married Mickey Minnie) (married Mickey Minnie)))
(check (run-query-bounded looping-disjunction 300)
       => '((or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
                (loop (Hacker Alyssa P)))
            diverged))

(define original-simple-query simple-query)
(set! simple-query louis-simple-query)

(check (run-query-bounded married-query 300) => '(diverged))
(check (run-query '(job ?x (computer programmer)))
       (=> same-elements?)
       '((job (Hacker Alyssa P) (computer programmer))
         (job (Fect Cy D) (computer programmer))))

(set! simple-query original-simple-query)
(put 'or 'qeval louis-disjoin)

(check (run-query-bounded looping-disjunction 300) => '(diverged))
