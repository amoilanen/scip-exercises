;; Open-coded primitives (exercise 5.38): calls of +, *, - and = are
;; compiled into machine operations on the registers arg1 and arg2.
;; Load after ch5/lib/compiler.scm.

;; The operands are evaluated left to right into arg1 and arg2.  The
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

;; = and - are open-coded with two operands; other calls of them, such as
;; (- x), are compiled as ordinary applications.
(define (compile-open-coded-binary exp target linkage)
  (end-with-linkage
   linkage
   (append-instruction-sequences
    (spread-arguments (operands exp))
    (make-instruction-sequence
     '(arg1 arg2) (list target)
     `((assign ,target (op ,(operator exp)) (reg arg1) (reg arg2)))))))

;; + and * take any number of operands: (+ a b c) is compiled as
;; (+ (+ a b) c), (+ a) as (+ 0 a) and (+) as 0.
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
