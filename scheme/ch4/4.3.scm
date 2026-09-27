(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/mceval.scm" (current-load-pathname)))

(define special-forms (make-strong-eqv-hash-table))

(define (put-special-form! tag handler)
  (hash-table-set! special-forms tag handler))

(define (special-form-handler exp)
  (and (pair? exp)
       (hash-table-ref/default special-forms (car exp) #f)))

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((special-form-handler exp)
         => (lambda (handler) (handler exp env)))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(put-special-form! 'quote
  (lambda (exp env) (text-of-quotation exp)))
(put-special-form! 'set! eval-assignment)
(put-special-form! 'define eval-definition)
(put-special-form! 'if eval-if)
(put-special-form! 'lambda
  (lambda (exp env)
    (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
(put-special-form! 'begin
  (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put-special-form! 'cond
  (lambda (exp env) (mc-eval (cond->if exp) env)))

(check (interpret '(quote (a b))) => '(a b))
(check (interpret '(define x 1) '(set! x (+ x 1)) 'x) => 2)
(check (interpret '(if (< 1 2) 'yes 'no)) => 'yes)
(check (interpret '(begin 1 2 3)) => 3)
(check (interpret '(cond ((= 1 2) 'a) ((= 1 1) 'b) (else 'c))) => 'b)
(check (interpret '(define (factorial n)
                     (if (= n 0) 1 (* n (factorial (- n 1)))))
                  '(factorial 6))
       => 720)
(check-error (interpret 'undefined-variable))

;; A new special form needs only a table entry, not a change to mc-eval.
(put-special-form! 'unless
  (lambda (exp env)
    (if (true? (mc-eval (cadr exp) env))
        false
        (eval-sequence (cddr exp) env))))

(check (interpret '(unless (= 1 2) 'ran)) => 'ran)
(check (interpret '(unless (= 1 1) (car '()))) => #f)
