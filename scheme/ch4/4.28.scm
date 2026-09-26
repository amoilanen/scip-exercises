(load "lib/check.scm")
(load "ch4/lib/lazy.scm")

;; A procedure passed as an argument reaches the callee as a thunk.  When the
;; callee applies it, the operator is a variable whose value is that thunk,
;; and mc-apply cannot apply a thunk: it is neither a primitive nor a
;; compound procedure.  Hence the operator is forced with actual-value.

(check (interpret '(define (apply-to-one f) (f 1))
                  '(apply-to-one (lambda (x) (+ x 1))))
       => 2)

(define callee-env
  (extend-environment '(f)
                      (list (delay-it '(lambda (x) (+ x 1))
                                      the-global-environment))
                      the-global-environment))

(check (thunk? (mc-eval 'f callee-env)) => #t)
(check-error (mc-apply (mc-eval 'f callee-env) '(1) callee-env))
(check (mc-apply (actual-value 'f callee-env) '(1) callee-env) => 2)
