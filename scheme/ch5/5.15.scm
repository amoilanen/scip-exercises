(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

;; The counting machine wraps the execution procedure of every installed
;; instruction and delegates all other requests to the original machine.

(define make-uncounted-machine make-new-machine)

(define (make-new-machine)
  (let ((machine (make-uncounted-machine))
        (instruction-count 0))
    (define (count-executions! inst)
      (let ((execute (instruction-execution-proc inst)))
        (set-instruction-execution-proc!
         inst
         (lambda ()
           (set! instruction-count (+ instruction-count 1))
           (execute)))))
    (define (print-and-reset-count)
      (newline)
      (display (list 'instruction-count '= instruction-count))
      (set! instruction-count 0))
    (lambda (message)
      (case message
        ((install-instruction-sequence)
         (lambda (seq)
           (for-each count-executions! seq)
           ((machine 'install-instruction-sequence) seq)))
        ((instruction-count) instruction-count)
        ((print-instruction-count) (print-and-reset-count))
        (else (machine message))))))

(define factorial-machine (make-factorial-machine))

(check (with-output-to-string
         (lambda ()
           (run-machine factorial-machine '((n 3)) 'val)
           (factorial-machine 'print-instruction-count)))
       => "\n(instruction-count = 27)")
(check (factorial-machine 'instruction-count) => 0)

(define (factorial-instruction-count n)
  (let ((machine (make-factorial-machine)))
    (run-machine machine (list (list 'n n)) 'val)
    (machine 'instruction-count)))

;; One instruction to start, 7 per recursive call, 4 for the base case and
;; 4 per return: 11n - 6 instructions in all.
(for-each (lambda (n)
            (check (factorial-instruction-count n) => (- (* 11 n) 6)))
          (iota 10 1))

(check (run-machine (make-gcd-machine) '((a 206) (b 40)) 'a) => 2)
