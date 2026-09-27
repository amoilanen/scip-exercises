(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/amb.scm" (current-load-pathname)))

(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((predicate-proc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (predicate-proc env
                      (lambda (predicate-value fail2)
                        (if (false? predicate-value)
                            (fail2)
                            (succeed 'ok fail2)))
                      fail))))

(install-special-form! 'require analyze-require)

;; require no longer needs to be a procedure: the special form works even
;; with the variable bound to something else.
(define env (amb-environment '(define require 'not-a-procedure)))

(check (amb-collect '(let ((x (an-element-of '(1 2 3 4 5 6))))
                       (require (even? x))
                       x)
                    env)
       => '(2 4 6))
(check (amb-collect '(require true) env) => '(ok))
(check (amb-collect '(require false) env) => '())
(check (amb-collect '(let ((x (amb 1 2 3)))
                       (require (> x 1))
                       (require (< x 3))
                       x)
                    env)
       => '(2))
