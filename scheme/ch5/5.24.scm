(load "lib/check.scm")
(load "ch5/lib/eceval.scm")

(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (no-more-clauses? clauses) (null? clauses))

;; The caller's continuation stays on the stack while the clauses are tried,
;; so the actions of the chosen clause can go straight to ev-sequence, which
;; keeps the last action a tail call.
(define cond-code
  '(ev-cond
    (save continue)
    (assign unev (op cond-clauses) (reg exp))
    ev-cond-loop
    (test (op no-more-clauses?) (reg unev))
    (branch (label ev-cond-no-clause))
    (assign exp (op first-clause) (reg unev))
    (test (op cond-else-clause?) (reg exp))
    (branch (label ev-cond-actions))
    (save exp)
    (save env)
    (save unev)
    (assign continue (label ev-cond-decide))
    (assign exp (op cond-predicate) (reg exp))
    (goto (label eval-dispatch))
    ev-cond-decide
    (restore unev)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-cond-actions))
    (assign unev (op rest-clauses) (reg unev))
    (goto (label ev-cond-loop))
    ev-cond-actions
    (assign unev (op cond-actions) (reg exp))
    (goto (label ev-sequence))
    ev-cond-no-clause
    (assign val (const #f))
    (restore continue)
    (goto (reg continue))))

(define cond-eceval
  (make-eceval (append eceval-dispatch-table '((cond? ev-cond)))
               cond-code
               (operation-entries 'cond? cond?
                                  'cond-clauses cond-clauses
                                  'first-clause first-clause
                                  'rest-clauses rest-clauses
                                  'no-more-clauses? no-more-clauses?
                                  'cond-else-clause? cond-else-clause?
                                  'cond-predicate cond-predicate
                                  'cond-actions cond-actions)))

(define (run . exps)
  (apply eceval-run cond-eceval exps))

(define sign
  '(define (sign x)
     (cond ((< x 0) 'negative)
           ((= x 0) 'zero)
           (else 'positive))))

(check (run sign '(list (sign -5) (sign 0) (sign 7)))
       => '(negative zero positive))
(check (run '(cond)) => #f)
(check (run '(cond ((= 1 2) 'no))) => #f)
(check (run '(cond (else 1 2 3))) => 3)
(check (run '(define x 1)
            '(cond ((= x 1) (set! x (+ x 1)) (* x 10))
                   (else 'no)))
       => 20)

(check (run '(define tested '())
            '(define (test! n result)
               (set! tested (cons n tested))
               result)
            '(cond ((test! 1 false) 'first)
                   ((test! 2 true) 'second)
                   ((test! 3 true) 'third))
            'tested)
       => '(2 1))

(define (count-down-depth n)
  (run '(define (count-down n)
          (cond ((= n 0) 'done)
                (else (count-down (- n 1)))))
       (list 'count-down n))
  (cdr (assq 'maximum-depth (stack-statistics cond-eceval))))

(check (count-down-depth 5) => (count-down-depth 50))
