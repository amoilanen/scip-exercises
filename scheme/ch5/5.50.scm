(load "lib/check.scm")
(load "ch5/lib/compiler.scm")

;;; The evaluator uses let, and and or, which the compiler must handle.

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (map car (let-bindings exp)) (let-body exp))
        (map cadr (let-bindings exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (and->if exps)
  (cond ((null? exps) 'true)
        ((null? (cdr exps)) (car exps))
        (else (make-if (car exps) (and->if (cdr exps)) 'false))))

;; The value of the first operand is kept in a variable that no expression
;; of the program can refer to.
(define (or->let exps)
  (cond ((null? exps) 'false)
        ((null? (cdr exps)) (car exps))
        (else
         (let ((value (generate-uninterned-symbol)))
           `(let ((,value ,(car exps)))
              ,(make-if value value (or->let (cdr exps))))))))

(define compile-without-derived-forms compile)

(define (compile exp target linkage)
  (cond ((let? exp) (compile (let->combination exp) target linkage))
        ((and? exp) (compile (and->if (cdr exp)) target linkage))
        ((or? exp) (compile (or->let (cdr exp)) target linkage))
        (else (compile-without-derived-forms exp target linkage))))

(check (compile-and-run '(let ((x 2) (y 3)) (* x y))) => 6)
(check (compile-and-run '(list (and) (and 1 2) (and 1 false 3))) => '(#t 2 #f))
(check (compile-and-run '(list (or) (or false 2) (or 1 (car '())))) => '(#f 2 1))
(check (compile-and-run '(define (f value) (or false value)) '(f 5)) => 5)
;;; The evaluator of section 4.1 is compiled from the definitions in
;;; ch4/lib/mceval.scm, except for driver-loop and interpret, which drive
;;; the evaluator from the underlying Scheme.

(define (read-definitions filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((definitions '()))
        (let ((definition (read)))
          (if (eof-object? definition)
              (reverse definitions)
              (loop (cons definition definitions))))))))

(define evaluator
  (remove (lambda (definition)
            (memq (definition-variable definition) '(driver-loop interpret)))
          (read-definitions "ch4/lib/mceval.scm")))

;; map takes procedures of the compiled program, which only compiled code
;; can call, so it is compiled too.
(define prelude
  '((define (map procedure items)
      (if (null? items)
          '()
          (cons (procedure (car items))
                (map procedure (cdr items)))))))

;; The primitives of the compiled evaluator are primitive procedures of the
;; machine, so its apply must apply those.
(define primitive-procedures
  (append primitive-procedures
          (list (list 'caddr caddr)
                (list 'cdddr cdddr)
                (list 'cadddr cadddr)
                (list 'caadr caadr)
                (list 'cdadr cdadr)
                (list 'string? string?)
                (list 'set-car! set-car!)
                (list 'set-cdr! set-cdr!)
                (list 'length length)
                (list 'error error)
                (list 'apply apply-primitive-procedure))))

(define (run-compiled-evaluator . exps)
  (apply compile-and-run
         (append prelude
                 evaluator
                 `((mc-eval '(begin ,@exps) the-global-environment)))))

(check (run-compiled-evaluator '(+ 1 2)) => 3)
(check (run-compiled-evaluator
        '(define (factorial n)
           (if (= n 1)
               1
               (* (factorial (- n 1)) n)))
        '(factorial 6))
       => 720)
