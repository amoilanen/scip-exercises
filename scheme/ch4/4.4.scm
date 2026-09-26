(load "lib/check.scm")
(load "ch4/lib/mceval.scm")

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (connective-operands exp) (cdr exp))

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;; As special forms

(define (eval-and exp env)
  (let loop ((exps (connective-operands exp)))
    (cond ((null? exps) true)
          ((last-exp? exps) (mc-eval (first-exp exps) env))
          ((false? (mc-eval (first-exp exps) env)) false)
          (else (loop (rest-exps exps))))))

(define (eval-or exp env)
  (let loop ((exps (connective-operands exp)))
    (if (null? exps)
        false
        (let ((value (mc-eval (first-exp exps) env)))
          (if (true? value)
              value
              (loop (rest-exps exps)))))))

(define (check-and-or)
  (check (interpret '(and)) => #t)
  (check (interpret '(and 1 2 3)) => 3)
  (check (interpret '(and 1 false 3)) => #f)
  (check (interpret '(and false (car '()))) => #f)
  (check (interpret '(or)) => #f)
  (check (interpret '(or false 2 3)) => 2)
  (check (interpret '(or false false)) => #f)
  (check (interpret '(or 1 (car '()))) => 1)
  (check (interpret '(define calls 0)
                    '(define (next!) (set! calls (+ calls 1)) calls)
                    '(or (next!) 'unused)
                    '(and (next!) (next!))
                    'calls)
         => 3))

(check-and-or)

;;; As derived expressions

(define (and->if exp)
  (let expand ((exps (connective-operands exp)))
    (cond ((null? exps) 'true)
          ((last-exp? exps) (first-exp exps))
          (else (make-if (first-exp exps)
                         (expand (rest-exps exps))
                         'false)))))

;; (or e1 e2 ...) must evaluate e1 only once without binding a name that the
;; remaining operands could see, so they are delayed in a thunk built in the
;; caller's environment:
;;   ((lambda (value rest) (if value value (rest))) e1 (lambda () (or e2 ...)))
(define (or->if exp)
  (let expand ((exps (connective-operands exp)))
    (if (null? exps)
        'false
        (list (make-lambda '(value rest)
                           (list (make-if 'value 'value '(rest))))
              (first-exp exps)
              (make-lambda '() (list (expand (rest-exps exps))))))))

(define (eval-and exp env) (mc-eval (and->if exp) env))
(define (eval-or exp env) (mc-eval (or->if exp) env))

(check (and->if '(and a b c)) => '(if a (if b c false) false))
(check-and-or)
(check (interpret '(define value false)
                  '(define rest 'outer)
                  '(or value rest))
       => 'outer)
