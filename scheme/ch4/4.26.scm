(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/mceval.scm" (current-load-pathname)))

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp) (cadddr exp))

(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((unless? exp) (mc-eval (unless->if exp) env))
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

(check (unless->if '(unless (= b 0) (/ a b) 'oops))
       => '(if (= b 0) 'oops (/ a b)))
(check (interpret '(unless false 'usual 'exceptional)) => 'usual)
(check (interpret '(unless true 'usual 'exceptional)) => 'exceptional)

;; As a special form, unless fixes the factorial of exercise 4.25 even in
;; an applicative-order evaluator.
(check (interpret '(define (factorial n)
                     (unless (= n 1)
                             (* n (factorial (- n 1)))
                             1))
                  '(factorial 5))
       => 120)

;; Alyssa's point: a special form is not a value, so it cannot be passed to
;; higher-order procedures.  A procedure unless could, for example, pick
;; element-wise between a list of defaults and a list of overrides:
;;   (map unless overridden? defaults overrides)
;; With the special form this needs a lambda wrapped around it.

(define map3-definition
  '(define (map3 f as bs cs)
     (if (null? as)
         '()
         (cons (f (car as) (car bs) (car cs))
               (map3 f (cdr as) (cdr bs) (cdr cs))))))

(check-error (interpret map3-definition
                        '(map3 unless
                               (list false true)
                               '(1 2)
                               '(a b))))
(check (interpret map3-definition
                  '(map3 (lambda (c u e) (unless c u e))
                         (list false true)
                         '(1 2)
                         '(a b)))
       => '(1 b))
