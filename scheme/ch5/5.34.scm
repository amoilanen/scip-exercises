(load "lib/check.scm")
(load "ch5/lib/compiler.scm")

(define iterative-factorial
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
     (iter 1 1)))

(define recursive-factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

;; The essential part of the compiled body of iter is the recursive call,
;; which is compiled with target val and linkage return:
;;
;;   (assign proc (op lookup-variable-value) (const iter) (reg env))
;;   (save continue)             ; continue, proc and env are saved only
;;   (save proc)                 ; while the operands are evaluated
;;   (save env)
;;   ...                         ; (+ counter 1) and (* counter product)
;;   (restore proc)
;;   (restore continue)          ; the stack is back to its depth on entry
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch))
;;   compiled-branch
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))            ; iter returns straight to our caller
;;
;; In the recursive factorial the recursive call is an operand of *, so it
;; is compiled with linkage next: continue, proc and argl stay saved while
;; it runs and are restored only when it returns, so the stack grows by
;; three entries per call.  In iter nothing is left on the stack when iter
;; calls itself, which makes the process iterative.

(define (maximum-depth . exps)
  (let ((machine (apply make-compiled-machine exps)))
    (start machine)
    (cdr (assq 'maximum-depth (stack-statistics machine)))))

(check (compile-and-run iterative-factorial '(factorial 10)) => 3628800)
(check (maximum-depth iterative-factorial '(factorial 5))
       => (maximum-depth iterative-factorial '(factorial 20)))
(check (maximum-depth recursive-factorial '(factorial 20))
       => (+ (maximum-depth recursive-factorial '(factorial 5)) (* 3 15)))
