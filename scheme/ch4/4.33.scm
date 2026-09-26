(load "lib/check.scm")
(load "ch4/lib/lazy.scm")

;; A quoted pair is turned into a call of the evaluated language's cons on
;; its quoted car and cdr, so nested and longer lists are converted only as
;; far as they are taken apart.

(define (quoted-pair? exp)
  (and (quoted? exp) (pair? (text-of-quotation exp))))

(define (quoted-pair->cons exp)
  (let ((pair (text-of-quotation exp)))
    (list 'cons
          (list 'quote (car pair))
          (list 'quote (cdr pair)))))

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted-pair? exp) (mc-eval (quoted-pair->cons exp) env))
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

(define (run . exps)
  (apply interpret (append lazy-list-definitions exps)))

(check (quoted-pair->cons ''(a b c)) => '(cons 'a '(b c)))
(check (run ''a) => 'a)
(check (run ''()) => '())
(check (run '(car '(a b c))) => 'a)
(check (run '(car (cdr '(a b c)))) => 'b)
(check (run '(null? (cdr (cdr '(a b))))) => #t)
(check (run '(cdr '(a . b))) => 'b)
(check (run '(car (car (cdr '(a (b c)))))) => 'b)
(check (run '(list-ref (add-lists '(1 2 3) integers) 2)) => 6)
