(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

;;; a. In afterfib-n-2, Fib(n - 2) is in val and Fib(n - 1) on the stack.
;;; Restoring the saved value straight into n leaves the two numbers swapped,
;;; which does not matter for their sum, and saves an instruction.

(define shorter-fib-controller
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
      (restore n)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
    fib-done))

(define (make-shorter-fib-machine)
  (make-machine '(n val continue)
                arithmetic-operations
                shorter-fib-controller))

(define (fib-values machine-maker)
  (map (lambda (n) (run-machine (machine-maker) (list (list 'n n)) 'val))
       '(0 1 2 5 10)))

(check (fib-values make-shorter-fib-machine) => '(0 1 1 5 55))

(define swapping-controller
  '((assign x (const 1))
    (assign y (const 2))
    (save x)
    (save y)
    (restore x)
    (restore y)))

(define (make-swapping-machine)
  (make-machine '(x y) '() swapping-controller))

(define (swapped-registers)
  (let ((machine (make-swapping-machine)))
    (start machine)
    (list (get-register-contents machine 'x)
          (get-register-contents machine 'y))))

;; With a single stack, restoring into another register moves values around.
(check (swapped-registers) => '(2 1))

;;; b. Save the register name with the value and refuse to restore it into
;;; another register.

(define (make-save inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (push stack (cons name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name)))
    (lambda ()
      (let ((saved (pop stack)))
        (if (not (eq? (car saved) name))
            (error "Value saved from another register -- RESTORE"
                   (car saved) name))
        (set-contents! reg (cdr saved))
        (advance-pc pc)))))

(check-error (swapped-registers))
(check-error (fib-values make-shorter-fib-machine))
(check (fib-values make-fib-machine) => '(0 1 1 5 55))
(check (run-machine (make-factorial-machine) '((n 5)) 'val) => 120)

;;; c. A separate stack for each register. The machine's stack becomes a
;;; family of stacks, one for each register that is saved or restored.

(define make-single-stack make-stack)

(define (make-stack)
  (let ((stacks '()))
    (define (stack-for name)
      (let ((entry (assq name stacks)))
        (if entry
            (cdr entry)
            (let ((stack (make-single-stack)))
              (set! stacks (cons (cons name stack) stacks))
              stack))))
    (define (ask-each-stack message)
      (map (lambda (entry) (cons (car entry) ((cdr entry) message)))
           stacks))
    (lambda (message)
      (case message
        ((for) stack-for)
        ((initialize) (ask-each-stack 'initialize) 'done)
        ((statistics) (ask-each-stack 'statistics))
        ((print-statistics)
         (newline)
         (display (ask-each-stack 'statistics)))
        (else (error "Unknown request -- STACK" message))))))

(define (make-save inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name))
         (register-stack ((stack 'for) name)))
    (lambda ()
      (push register-stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((name (stack-inst-reg-name inst))
         (reg (get-register machine name))
         (register-stack ((stack 'for) name)))
    (lambda ()
      (set-contents! reg (pop register-stack))
      (advance-pc pc))))

(check (swapped-registers) => '(1 2))
(check (fib-values make-fib-machine) => '(0 1 1 5 55))

(let ((machine (make-factorial-machine)))
  (check (run-machine machine '((n 5)) 'val) => 120)
  (check (stack-statistics machine)
         => '((n (total-pushes . 4) (maximum-depth . 4))
              (continue (total-pushes . 4) (maximum-depth . 4)))))
