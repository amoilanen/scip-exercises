(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; cons-stream does not evaluate its second operand until the stream's cdr
;; is taken.  Without the let, that operand is the variable the-assertions,
;; which by then refers to the new stream itself: the result is an infinite
;; stream repeating the newest assertion, and the old ones are lost.  The
;; let evaluates the old stream first and the delayed cdr refers to it.

(define (add-assertion-without-let! assertion)
  (set! the-assertions (cons-stream assertion the-assertions))
  'ok)

(reset-data-base!)
(add-assertion-without-let! '(job (Hacker Alyssa P) (computer programmer)))
(add-assertion-without-let! '(job (Fect Cy D) (computer programmer)))

(check (stream-head the-assertions 3)
       => '((job (Fect Cy D) (computer programmer))
            (job (Fect Cy D) (computer programmer))
            (job (Fect Cy D) (computer programmer))))

;; A pattern starting with a variable cannot use the index and scans
;; the-assertions, so it never runs out of answers.
(check (run-query-head '(?relation ?x (computer programmer)) 3)
       => '((job (Fect Cy D) (computer programmer))
            (job (Fect Cy D) (computer programmer))
            (job (Fect Cy D) (computer programmer))))

(reset-data-base!)
(add-assertion! '(job (Hacker Alyssa P) (computer programmer)))
(add-assertion! '(job (Fect Cy D) (computer programmer)))

(check (run-query '(?relation ?x (computer programmer)))
       => '((job (Fect Cy D) (computer programmer))
            (job (Hacker Alyssa P) (computer programmer))))
