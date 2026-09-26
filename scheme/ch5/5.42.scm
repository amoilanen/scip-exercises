(load "lib/check.scm")
(load "ch5/5.41.scm")

;; A variable that is not in the compile-time environment can only be in the
;; global environment, at the end of the run-time environment; it is found
;; by name as before.

(define compile-variable-by-name compile-variable)
(define compile-assignment-by-name compile-assignment)

(define (compile-variable exp target linkage)
  (let ((address (find-variable exp (compile-time-environment))))
    (if (eq? address 'not-found)
        (compile-variable-by-name exp target linkage)
        (end-with-linkage
         linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,address)
                    (reg env))))))))

(define (compile-assignment exp target linkage)
  (let ((address (find-variable (assignment-variable exp)
                                (compile-time-environment))))
    (if (eq? address 'not-found)
        (compile-assignment-by-name exp target linkage)
        (end-with-linkage
         linkage
         (preserving
          '(env)
          (compile (assignment-value exp) 'val 'next)
          (make-instruction-sequence
           '(env val) (list target)
           `((perform (op lexical-address-set!)
                      (const ,address)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))))))))

(define compiled-code-operations
  (append (list (list 'lexical-address-lookup lexical-address-lookup)
                (list 'lexical-address-set! lexical-address-set!))
          compiled-code-operations))

(define nested-lambdas
  '((lambda (x y)
      (lambda (a b c d e)
        ((lambda (y z) (* x y z))
         (* a b x)
         (+ c d x))))
    3
    4))

(define (variable-accesses code)
  (filter-map
   (lambda (inst)
     (and (pair? inst)
          (let ((operation (find operation-exp? (list (cddr inst) (cdr inst)))))
            (and operation
                 (memq (operation-exp-op operation)
                       '(lexical-address-lookup lexical-address-set!
                         lookup-variable-value set-variable-value!))
                 (constant-exp-value (car (operation-exp-operands operation)))))))
   code))

;; Operands are compiled right to left.
(check (variable-accesses (statements (compile nested-lambdas 'val 'next)))
       => '((0 0) (0 1) (2 0) * (0 3) (0 2) + (2 0) (0 1) (0 0) *))

(check (compile-and-run (list nested-lambdas 1 2 3 4 5)) => 180)

(check (compile-and-run
        '(define (make-counter)
           (let-counter 0))
        '(define (let-counter count)
           (lambda ()
             (set! count (+ count 1))
             count))
        '(define counter (make-counter))
        '(counter)
        '(counter))
       => 2)

(check (compile-and-run
        '(define total 0)
        '(define (add! x) (set! total (+ total x)))
        '(add! 3)
        '(add! 4)
        'total)
       => 7)

(check (compile-and-run
        '(define (factorial n)
           (if (= n 1)
               1
               (* (factorial (- n 1)) n)))
        '(factorial 6))
       => 720)
