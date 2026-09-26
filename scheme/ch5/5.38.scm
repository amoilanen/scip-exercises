(load "lib/check.scm")
(load "ch5/lib/compiler.scm")
(load "ch5/lib/open-coding.scm")

;; a, b and d: spread-arguments and the code generators are in
;; ch5/lib/open-coding.scm, which later exercises share.

(define (compiled exp)
  (statements (compile exp 'val 'next)))

(check (compiled '(+ 1 x))
       => '((assign arg1 (const 1))
            (assign arg2 (op lookup-variable-value) (const x) (reg env))
            (assign val (op +) (reg arg1) (reg arg2))))
(check (compiled '(- (* a 2) (+ b 1)))
       => '((assign arg1 (op lookup-variable-value) (const a) (reg env))
            (assign arg2 (const 2))
            (assign arg1 (op *) (reg arg1) (reg arg2))
            (save arg1)
            (assign arg1 (op lookup-variable-value) (const b) (reg env))
            (assign arg2 (const 1))
            (assign arg2 (op +) (reg arg1) (reg arg2))
            (restore arg1)
            (assign val (op -) (reg arg1) (reg arg2))))
(check (compiled '(+ 1 2 3))
       => '((assign arg1 (const 1))
            (assign arg2 (const 2))
            (assign arg1 (op +) (reg arg1) (reg arg2))
            (assign arg2 (const 3))
            (assign val (op +) (reg arg1) (reg arg2))))
(check (compiled '(*)) => '((assign val (const 1))))

(check (compile-and-run '(+ 1 2 3 4)) => 10)
(check (compile-and-run '(* 2 3 4)) => 24)
(check (compile-and-run '(+)) => 0)
(check (compile-and-run '(* 7)) => 7)
(check (compile-and-run '(- 3)) => -3)
(check (compile-and-run '(- (* 2 5) (+ 1 1))) => 8)
(check (compile-and-run '(define (f x) (* x 2)) '(+ (f 1) (f 2) (f 3))) => 12)
(check-error (compile-and-run '(+ 1 'a)))

;; c. The primitives are no longer looked up and applied, which leaves only
;; the recursive call.  Around it only continue and env are saved, instead
;; of continue, env, proc and argl.
(define factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define (compiled-without-open-coding exp)
  (fluid-let ((open-coded? (lambda (exp) false)))
    (compiled exp)))

(define (looked-up-variables code)
  (filter-map (lambda (inst)
                (and (tagged-list? inst 'assign)
                     (equal? (caddr inst) '(op lookup-variable-value))
                     (constant-exp-value (cadddr inst))))
              code))

(define (saved-registers code)
  (filter-map (lambda (inst) (and (tagged-list? inst 'save) (cadr inst)))
              code))

(check (looked-up-variables (compiled factorial)) => '(n factorial n n))
(check (looked-up-variables (compiled-without-open-coding factorial))
       => '(= n * n factorial - n))
(check (saved-registers (compiled factorial)) => '(continue env))
(check (saved-registers (compiled-without-open-coding factorial))
       => '(continue env continue proc argl proc))
(check (compile-and-run factorial '(factorial 6)) => 720)
