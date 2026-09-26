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

(define (variable-lookups code)
  (filter-map
   (lambda (inst)
     (and (tagged-list? inst 'assign)
          (operation-exp? (cddr inst))
          (memq (operation-exp-op (cddr inst))
                '(lexical-address-lookup lookup-variable-value))
          (constant-exp-value (car (operation-exp-operands (cddr inst))))))
   code))

;; Operands are compiled right to left; the body of the innermost lambda
;; comes first because it is tacked on right after its make-compiled-procedure.
(check (variable-lookups (statements (compile nested-lambdas 'val 'next)))
       => '(* (0 1) (0 0) (2 0)
            + (1 0) (0 3) (0 2)
            * (1 0) (0 1) (0 0)))

(check (compile-and-run (list nested-lambdas 1 2 3 4 5)) => 180)

(check (let ((code (statements (compile '(lambda (n) (set! n 1)) 'val 'next))))
         (and (member '(perform (op lexical-address-set!)
                                (const (0 0))
                                (reg val)
                                (reg env))
                      code)
              #t))
       => #t)

(check (compile-and-run
        '(define (make-counter count)
           (lambda ()
             (set! count (+ count 1))
             count))
        '(define counter (make-counter 10))
        '(counter)
        '(counter))
       => 12)

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
