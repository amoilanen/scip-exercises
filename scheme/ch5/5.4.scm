(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

;; a. Recursive exponentiation. Registers b, n, val, continue and a stack;
;; b never changes and n is not needed after the recursive call, so only
;; continue is saved.

(define expt-recursive-controller
  '((assign continue (label expt-done))
    expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
    after-expt
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
    base-case
      (assign val (const 1))
      (goto (reg continue))
    expt-done))

(define (make-expt-recursive-machine)
  (make-machine '(b n val continue)
                arithmetic-operations
                expt-recursive-controller))

;; b. Iterative exponentiation. Registers b, n, counter and product; no stack.

(define expt-iterative-controller
  '((assign counter (reg n))
    (assign product (const 1))
    expt-loop
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))
      (assign counter (op -) (reg counter) (const 1))
      (assign product (op *) (reg b) (reg product))
      (goto (label expt-loop))
    expt-done))

(define (make-expt-iterative-machine)
  (make-machine '(b n counter product)
                arithmetic-operations
                expt-iterative-controller))

(define (expt-recursive b n)
  (run-machine (make-expt-recursive-machine)
               (list (list 'b b) (list 'n n))
               'val))

(define (expt-iterative b n)
  (run-machine (make-expt-iterative-machine)
               (list (list 'b b) (list 'n n))
               'product))

(check (expt-recursive 2 10) => 1024)
(check (expt-iterative 2 10) => 1024)
