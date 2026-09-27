(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/mceval.scm" (current-load-pathname)))

;; Louis's evaluator: applications are recognized before assignments.
(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
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
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; a. Every pair is taken for an application, so (define x 3) becomes a call
;; of the procedure named define with the argument x: both are looked up as
;; variables and neither is bound.
(define (error-message thunk)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (condition) (k (condition/report-string condition)))
      thunk))))

(check-error (interpret '(define x 3)))
(check (error-message (lambda () (interpret '(if true 1 2))))
       => "Unbound variable if")

;; b. With applications tagged by call, the early clause only claims them.
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(check (interpret '(define x 3) 'x) => 3)
(check (interpret '(call + 1 2)) => 3)
(check (interpret '(call (lambda () 42))) => 42)
(check (interpret '(define (factorial n)
                     (if (call = n 0)
                         1
                         (call * n (call factorial (call - n 1)))))
                  '(call factorial 5))
       => 120)
(check-error (interpret '(+ 1 2)))
