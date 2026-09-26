;; The lazy evaluator of section 4.2.2, built on the metacircular evaluator.
;; Compound procedures receive their operands as thunks; primitives, the
;; predicate of if, operators and the driver loop force them.

(load "ch4/lib/mceval.scm")

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((application? exp)
         (mc-apply (actual-value (operator exp) env)
                   (operands exp)
                   env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (let ((value (actual-value (first-operand exps) env)))
        (cons value (list-of-arg-values (rest-operands exps) env)))))

(define (list-of-delayed-args exps env)
  (map (lambda (exp) (delay-it exp env)) exps))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

;;; Thunks

(define (actual-value exp env)
  (force-it (mc-eval exp env)))

(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;; A forced thunk becomes an evaluated thunk and drops its environment,
;; which may then be garbage collected.
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;;; Running the evaluator

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; Like the driver loop, forces the value of every expression.
(define (interpret . exps)
  (let ((env (setup-environment)))
    (let loop ((exps exps) (value 'ok))
      (if (null? exps)
          value
          (loop (cdr exps) (actual-value (car exps) env))))))

;;; Lazy lists of section 4.2.3, as definitions for the evaluated language

(define lazy-list-definitions
  '((define (cons x y) (lambda (m) (m x y)))
    (define (car z) (z (lambda (p q) p)))
    (define (cdr z) (z (lambda (p q) q)))
    (define (list-ref items n)
      (if (= n 0)
          (car items)
          (list-ref (cdr items) (- n 1))))
    (define (map proc items)
      (if (null? items)
          '()
          (cons (proc (car items))
                (map proc (cdr items)))))
    (define (scale-list items factor)
      (map (lambda (x) (* x factor)) items))
    (define (add-lists list1 list2)
      (cond ((null? list1) list2)
            ((null? list2) list1)
            (else (cons (+ (car list1) (car list2))
                        (add-lists (cdr list1) (cdr list2))))))
    (define ones (cons 1 ones))
    (define integers (cons 1 (add-lists ones integers)))))
