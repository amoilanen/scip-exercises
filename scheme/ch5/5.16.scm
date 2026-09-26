(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

;; Like the counting machine of exercise 5.15, the tracing machine wraps the
;; execution procedures of the installed instructions.

(define (trace-instruction inst)
  (write (instruction-text inst))
  (newline))

(define make-untraced-machine make-new-machine)

(define (make-new-machine)
  (let ((machine (make-untraced-machine))
        (tracing? #f))
    (define (trace-executions! inst)
      (let ((execute (instruction-execution-proc inst)))
        (set-instruction-execution-proc!
         inst
         (lambda ()
           (if tracing? (trace-instruction inst))
           (execute)))))
    (lambda (message)
      (case message
        ((install-instruction-sequence)
         (lambda (seq)
           (for-each trace-executions! seq)
           ((machine 'install-instruction-sequence) seq)))
        ((trace-on) (set! tracing? #t) 'done)
        ((trace-off) (set! tracing? #f) 'done)
        (else (machine message))))))

(define (trace-output machine inputs)
  (with-output-to-string
    (lambda () (run-machine machine inputs 'val))))

(define (lines . strings)
  (apply string-append
         (map (lambda (string) (string-append string "\n")) strings)))

(define factorial-machine (make-factorial-machine))

(check (trace-output factorial-machine '((n 2))) => "")

(factorial-machine 'trace-on)
(check (trace-output factorial-machine '((n 2)))
       => (lines "(assign continue (label fact-done))"
                 "(test (op =) (reg n) (const 1))"
                 "(branch (label base-case))"
                 "(save continue)"
                 "(save n)"
                 "(assign n (op -) (reg n) (const 1))"
                 "(assign continue (label after-fact))"
                 "(goto (label fact-loop))"
                 "(test (op =) (reg n) (const 1))"
                 "(branch (label base-case))"
                 "(assign val (const 1))"
                 "(goto (reg continue))"
                 "(restore n)"
                 "(restore continue)"
                 "(assign val (op *) (reg n) (reg val))"
                 "(goto (reg continue))"))
(check (get-register-contents factorial-machine 'val) => 2)

(factorial-machine 'trace-off)
(check (trace-output factorial-machine '((n 5))) => "")
(check (get-register-contents factorial-machine 'val) => 120)
