(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

;; In afterfib-n-1 the controller restores continue and saves it again right
;; away, while continue is not changed in between. Both instructions can go.

(define fib-controller-without-extra-save
  '((assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
    afterfib-n-1
      (restore n)
      (assign n (op -) (reg n) (const 2))
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
    afterfib-n-2
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
    fib-done))

(define (fib-with-statistics machine n)
  (let ((value (run-machine machine (list (list 'n n)) 'val)))
    (cons value (stack-statistics machine))))

(define (make-improved-fib-machine)
  (make-machine '(n val continue)
                arithmetic-operations
                fib-controller-without-extra-save))

(check (map (lambda (n) (run-machine (make-improved-fib-machine)
                                     (list (list 'n n))
                                     'val))
            '(0 1 2 3 4 5 10))
       => '(0 1 1 2 3 5 55))

;; Fib(10) makes 88 calls with n >= 2; each of them now pushes three values
;; instead of four.
(check (fib-with-statistics (make-fib-machine) 10)
       => '(55 (total-pushes . 352) (maximum-depth . 18)))
(check (fib-with-statistics (make-improved-fib-machine) 10)
       => '(55 (total-pushes . 264) (maximum-depth . 18)))
