(load "lib/check.scm")
(load "ch4/lib/mceval.scm")

;; found receives the list of values whose car is the variable's value;
;; not-found is called without arguments.

(define (scan-frame var frame found not-found)
  (let scan ((vars (frame-variables frame))
             (vals (frame-values frame)))
    (cond ((null? vars) (not-found))
          ((eq? var (car vars)) (found vals))
          (else (scan (cdr vars) (cdr vals))))))

(define (scan-environment var env found not-found)
  (let env-loop ((env env))
    (if (eq? env the-empty-environment)
        (not-found)
        (scan-frame var
                    (first-frame env)
                    found
                    (lambda () (env-loop (enclosing-environment env)))))))

(define (lookup-variable-value var env)
  (scan-environment var env
                    car
                    (lambda () (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (scan-environment var env
                    (lambda (vals) (set-car! vals val))
                    (lambda () (error "Unbound variable -- SET!" var))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame var frame
                (lambda (vals) (set-car! vals val))
                (lambda () (add-binding-to-frame! var val frame)))))

(check (interpret '(define x 1) '(define x 2) 'x) => 2)
(check (interpret '(define x 1) '(set! x (+ x 1)) 'x) => 2)
(check (interpret '(define x 'outer)
                  '(define (f x) (set! x 'changed) x)
                  '(list (f 'inner) x))
       => '(changed outer))
(check (interpret '(define (factorial n)
                     (if (= n 0) 1 (* n (factorial (- n 1)))))
                  '(factorial 5))
       => 120)
(check-error (interpret 'unbound))
(check-error (interpret '(set! unbound 1)))
