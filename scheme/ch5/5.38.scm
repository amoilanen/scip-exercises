(load "lib/check.scm")
(load "ch5/lib/compiler.scm")

;; a. The operands are evaluated left to right into arg1 and arg2.  The
;; empty sequence that needs arg1 makes preserving save arg1 around the
;; second operand when that operand changes it, e.g. (+ a (* b c)).
(define (spread-arguments operands)
  (let ((arg1-code (compile (car operands) 'arg1 'next))
        (arg2-code (compile (cadr operands) 'arg2 'next)))
    (preserving '(env)
                arg1-code
                (preserving '(arg1)
                            arg2-code
                            (make-instruction-sequence '(arg1) '() '())))))

;; b. = and - are open-coded with two operands; other calls of them, such as
;; (- x), are compiled as ordinary applications.
(define (compile-open-coded-binary exp target linkage)
  (end-with-linkage
   linkage
   (append-instruction-sequences
    (spread-arguments (operands exp))
    (make-instruction-sequence
     '(arg1 arg2) (list target)
     `((assign ,target (op ,(operator exp)) (reg arg1) (reg arg2)))))))

;; d. (+ a b c) is compiled as (+ (+ a b) c), (+ a) as (+ 0 a) and (+) as 0.
(define (compile-open-coded-accumulation exp identity target linkage)
  (let ((op (operator exp))
        (args (operands exp)))
    (cond ((null? args) (compile identity target linkage))
          ((null? (cdr args))
           (compile (list op identity (car args)) target linkage))
          ((null? (cddr args))
           (compile-open-coded-binary exp target linkage))
          (else
           (compile (cons op (cons (list op (car args) (cadr args))
                                   (cddr args)))
                    target
                    linkage)))))

(define accumulation-identities '((+ . 0) (* . 1)))
(define binary-open-coded-operators '(= -))

(define (open-coded? exp)
  (and (pair? exp)
       (or (assq (operator exp) accumulation-identities)
           (and (memq (operator exp) binary-open-coded-operators)
                (= (length (operands exp)) 2)))))

(define (compile-open-coded exp target linkage)
  (let ((identity (assq (operator exp) accumulation-identities)))
    (if identity
        (compile-open-coded-accumulation exp (cdr identity) target linkage)
        (compile-open-coded-binary exp target linkage))))

(define compile-without-open-coding compile)

(define (compile exp target linkage)
  (if (open-coded? exp)
      (compile-open-coded exp target linkage)
      (compile-without-open-coding exp target linkage)))

;; Procedure calls may change the argument registers too.
(define all-regs (append all-regs '(arg1 arg2)))

(define compiled-code-operations
  (append (list (list '+ +) (list '- -) (list '* *) (list '= =))
          compiled-code-operations))

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
