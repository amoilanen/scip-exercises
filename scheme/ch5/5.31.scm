(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/compiler.scm" (current-load-pathname)))

;; The evaluator saves env around the operator, env around each operand but
;; the last, argl around each operand, and proc around the operand sequence.
;;
;; (f 'x 'y)      all of them are superfluous: neither a variable nor a
;;                constant changes a register.
;; ((f) 'x 'y)    all superfluous: (f) clobbers env, but the constant
;;                operands don't need it.
;; (f (g 'x) y)   env around the operator is superfluous; env around (g 'x)
;;                is needed for y, and argl and proc are needed because the
;;                call to g changes them.
;; (f (g 'x) 'y)  env saves are superfluous; argl and proc are needed.
;;
;; The compiler keeps only the saves that are needed.  It evaluates operands
;; right to left, so for (f (g 'x) y) it looks up y before calling g and
;; doesn't have to save env either.

(define (saved-registers exp)
  (filter-map (lambda (inst) (and (tagged-list? inst 'save) (cadr inst)))
              (statements (compile exp 'val 'next))))

(check (saved-registers '(f 'x 'y)) => '())
(check (saved-registers '((f) 'x 'y)) => '())
(check (saved-registers '(f (g 'x) y)) => '(proc argl))
(check (saved-registers '(f (g 'x) 'y)) => '(proc argl))

;; When an operand evaluated after the call needs env, env is saved as well.
(check (saved-registers '(f x (g 'y))) => '(proc env))
(check (saved-registers '((f) x)) => '(env))
