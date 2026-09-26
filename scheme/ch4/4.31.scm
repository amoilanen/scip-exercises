(load "lib/check.scm")
(load "ch4/lib/lazy.scm")

;; Parameters are strict unless declared (name lazy) or (name lazy-memo).
;; The lazy evaluator's forcing of operators, primitive arguments and if
;; predicates stays; it has no effect on values that are not thunks.

(define (lazy-parameter? parameter)
  (and (pair? parameter) (eq? (cadr parameter) 'lazy)))
(define (lazy-memo-parameter? parameter)
  (and (pair? parameter) (eq? (cadr parameter) 'lazy-memo)))

(define (parameter-name parameter)
  (if (pair? parameter) (car parameter) parameter))

(define (delay-it-without-memo exp env) (list 'unmemoized-thunk exp env))
(define (unmemoized-thunk? obj) (tagged-list? obj 'unmemoized-thunk))

(define memoizing-force-it force-it)

(define (force-it obj)
  (if (unmemoized-thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      (memoizing-force-it obj)))

(define (declared-argument parameter operand env)
  (cond ((symbol? parameter) (actual-value operand env))
        ((lazy-parameter? parameter) (delay-it-without-memo operand env))
        ((lazy-memo-parameter? parameter) (delay-it operand env))
        (else (error "Unknown parameter declaration" parameter))))

(define (list-of-declared-args parameters operands env)
  (cond ((and (null? parameters) (no-operands? operands)) '())
        ((or (null? parameters) (no-operands? operands))
         (error "Wrong number of arguments -- APPLY" parameters operands))
        (else
         (let ((argument (declared-argument (car parameters)
                                            (first-operand operands)
                                            env)))
           (cons argument
                 (list-of-declared-args (cdr parameters)
                                        (rest-operands operands)
                                        env))))))

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (let ((parameters (procedure-parameters procedure)))
           (eval-sequence
            (procedure-body procedure)
            (extend-environment
             (map parameter-name parameters)
             (list-of-declared-args parameters arguments env)
             (procedure-environment procedure)))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(check (interpret '(define (f a (b lazy) c (d lazy-memo)) (list a c))
                  '(f 1 (car '()) 3 (car '())))
       => '(1 3))
(check-error (interpret '(define (f x) 'ignored)
                        '(f (car '()))))
(check (interpret '(define (try a (b lazy)) (if (= a 0) 1 b))
                  '(try 0 (/ 1 0)))
       => 1)
(check (interpret '(define (unless condition
                                   (usual-value lazy)
                                   (exceptional-value lazy))
                     (if condition exceptional-value usual-value))
                  '(define (factorial n)
                     (unless (= n 1)
                             (* n (factorial (- n 1)))
                             1))
                  '(factorial 5))
       => 120)

(define (count-evaluations definition call)
  (interpret '(define count 0)
             '(define (id x)
                (set! count (+ count 1))
                x)
             definition
             call
             'count))

(check (count-evaluations '(define (twice x) (+ x x))
                          '(twice (id 1)))
       => 1)
(check (count-evaluations '(define (twice (x lazy)) (+ x x))
                          '(twice (id 1)))
       => 2)
(check (count-evaluations '(define (twice (x lazy-memo)) (+ x x))
                          '(twice (id 1)))
       => 1)
(check (count-evaluations '(define (ignore (x lazy)) 'ignored)
                          '(ignore (id 1)))
       => 0)

(check-error (interpret '(define (f (x eager)) x) '(f 1)))
(check-error (interpret '(define (f a (b lazy)) a) '(f 1)))
(check-error (interpret '(define (f a) a) '(f 1 2)))
