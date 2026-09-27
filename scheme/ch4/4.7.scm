(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "4.6.scm" (current-load-pathname)))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (let nest ((bindings (let-bindings exp)))
    (if (or (null? bindings) (null? (cdr bindings)))
        (make-let bindings (let-body exp))
        (make-let (list (car bindings))
                  (list (nest (cdr bindings)))))))

;; Adding this clause is enough: the nested lets are evaluated by the let
;; clause of 4.6, so let* need not be expanded into lambdas directly.
(define eval-without-let* mc-eval)

(define (mc-eval exp env)
  (if (let*? exp)
      (mc-eval (let*->nested-lets exp) env)
      (eval-without-let* exp env)))

(check (let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))
       => '(let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z)))))
(check (let*->nested-lets '(let* () 1)) => '(let () 1))
(check (interpret '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))) => 39)
(check (interpret '(let* () 1 2)) => 2)
(check (interpret '(define x 10)
                  '(let* ((y x) (x 1)) (list x y)))
       => '(1 10))
