(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/analyze.scm" (current-load-pathname)))

;; The book's analyze-sequence walks the list of expressions once, during
;; analysis, and chains their execution procedures into a single procedure.
;; For one expression that procedure is the expression's own execution
;; procedure; for two it is (lambda (env) (proc1 env) (proc2 env)), which
;; just calls both.
;;
;; Alyssa's version keeps the list and walks it on every execution: with one
;; expression each run still calls execute-sequence and tests for the end of
;; the list; with two, it tests, calls, recurs and tests again. Part of the
;; work of analyzing the sequence is thus repeated at run time. The counter
;; below shows the loop steps growing with the number of runs.

(define sequence-steps 0)

(define (alyssa-analyze-sequence exps)
  (define (execute-sequence procs env)
    (set! sequence-steps (+ sequence-steps 1))
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

(define program
  '((define (f x) (set! x (+ x 1)) (* x 2))
    (define (g x) (+ x 1))
    (list (f 1) (f 2) (g 3))))

(check (apply analyzing-interpret program) => '(4 6 4))

(define analyze-sequence alyssa-analyze-sequence)

(check (apply analyzing-interpret program) => '(4 6 4))

(define (steps-to-run . exps)
  (set! sequence-steps 0)
  (apply analyzing-interpret exps)
  sequence-steps)

(check (steps-to-run '(define (one) 1) '(one)) => 1)
(check (steps-to-run '(define (one) 1) '(one) '(one) '(one)) => 3)
(check (steps-to-run '(define (two) 1 2) '(two)) => 2)
(check (steps-to-run '(define (two) 1 2) '(two) '(two) '(two)) => 6)
