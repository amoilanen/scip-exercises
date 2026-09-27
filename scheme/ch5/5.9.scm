(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

(define controllers-operating-on-labels
  '((start
       (assign a (op +) (label start) (const 1)))
    (start
       (test (op =) (reg a) (label start))
       (branch (label start)))
    (start
       (perform (op print) (label start)))))

(define (assemble-controller controller)
  (make-machine '(a)
                (cons (list 'print display) arithmetic-operations)
                controller))

;; The original assembler accepts them all.
(for-each assemble-controller controllers-operating-on-labels)

(define (make-operation-exp exp machine labels ops)
  (let ((op (lookup-prim (operation-exp-op exp) ops))
        (arg-procs
         (map (lambda (operand)
                (if (label-exp? operand)
                    (error "Operation applied to a label -- ASSEMBLE" exp)
                    (make-primitive-exp operand machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (arg-proc) (arg-proc)) arg-procs)))))

(for-each (lambda (controller)
            (check-error (assemble-controller controller)))
          controllers-operating-on-labels)

(check (run-machine (make-gcd-machine) '((a 206) (b 40)) 'a) => 2)
(check (run-machine (make-factorial-machine) '((n 5)) 'val) => 120)
