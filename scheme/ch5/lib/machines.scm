;; Machines of sections 5.1 and 5.2 used by several exercises, and a helper
;; for running them. Machines are built by calling make-machine when needed,
;; so exercises that redefine parts of the simulator see their changes.

(define arithmetic-operations
  (list (list '+ +) (list '- -) (list '* *) (list '/ /)
        (list '= =) (list '< <) (list '> >) (list 'rem remainder)))

(define (run-machine machine inputs output)
  (for-each (lambda (input)
              (set-register-contents! machine (car input) (cadr input)))
            inputs)
  ((machine 'stack) 'initialize)
  (start machine)
  (get-register-contents machine output))

(define gcd-controller
  '(test-b
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label test-b))
    gcd-done))

(define (make-gcd-machine)
  (make-machine '(a b t) arithmetic-operations gcd-controller))

;; Recursive factorial, figure 5.11.
(define factorial-controller
  '((assign continue (label fact-done))
    fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
    after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
    base-case
      (assign val (const 1))
      (goto (reg continue))
    fact-done))

(define (make-factorial-machine)
  (make-machine '(n val continue) arithmetic-operations factorial-controller))

;; Tree-recursive Fibonacci, figure 5.12.
(define fib-controller
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
      (restore continue)
      (assign n (op -) (reg n) (const 2))
      (save continue)
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

(define (make-fib-machine)
  (make-machine '(n val continue) arithmetic-operations fib-controller))
