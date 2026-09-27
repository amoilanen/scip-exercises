(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/mceval.scm" (current-load-pathname)))

;; New syntax without touching mc-eval or mc-apply, only the syntax procedures:
;;   (x := value)                     assignment written infix
;;   (if p then c else a), (if p then c)
;;   (fn (params ...) body ...)       instead of lambda
;; make-if and make-lambda change too, so the derived cond and procedure
;; definitions produce the new syntax.

(define (assignment? exp)
  (and (pair? exp)
       (pair? (cdr exp))
       (eq? (cadr exp) ':=)))
(define (assignment-variable exp) (car exp))
(define (assignment-value exp) (caddr exp))

(define (if-consequent exp) (cadddr exp))
(define (if-alternative exp)
  (let ((else-part (cddddr exp)))
    (if (null? else-part)
        'false
        (cadr else-part))))
(define (make-if predicate consequent alternative)
  (list 'if predicate 'then consequent 'else alternative))

(define (lambda? exp) (tagged-list? exp 'fn))
(define (make-lambda parameters body)
  (cons 'fn (cons parameters body)))

(check (interpret '(define x 1) '(x := (+ x 1)) 'x) => 2)
(check (interpret '(if (< 1 2) then 'yes else 'no)) => 'yes)
(check (interpret '(if (> 1 2) then 'yes else 'no)) => 'no)
(check (interpret '(if (> 1 2) then 'yes)) => #f)
(check (interpret '((fn (a b) (* a b)) 6 7)) => 42)
(check (interpret '(define (factorial n)
                     (if (= n 0) then 1 else (* n (factorial (- n 1)))))
                  '(factorial 5))
       => 120)
(check (interpret '(cond ((= 1 2) 'a) ((= 1 1) 'b) (else 'c))) => 'b)
(check (cond->if '(cond (p a) (else b))) => '(if p then a else b))
(check-error (interpret '(lambda (x) x)))
