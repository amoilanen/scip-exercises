(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

;; The machine keeps its labels, so that breakpoints can be given relative to
;; them, and a list of breakpoints, each an instruction with its label and
;; offset. Execution stops before a breakpoint instruction; proceeding
;; executes that instruction without stopping again.

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    ((machine 'install-labels) labels)
                    insts)))

(define (make-breakpoint inst label offset) (list inst label offset))
(define (breakpoint-label breakpoint) (cadr breakpoint))
(define (breakpoint-offset breakpoint) (caddr breakpoint))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (instruction-sequence '())
        (labels '())
        (breakpoints '()))
    (let ((operations
           (list (list 'initialize-stack (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register:" name)
            (set! register-table
                  (cons (list name (make-register name)) register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((entry (assoc name register-table)))
          (if entry
              (cadr entry)
              (error "Unknown register:" name))))
      (define (set-breakpoint label offset)
        (let ((insts (lookup-label labels label)))
          (if (not (<= 1 offset (length insts)))
              (error "No instruction at breakpoint:" label offset))
          (set! breakpoints
                (cons (make-breakpoint (list-ref insts (- offset 1))
                                       label
                                       offset)
                      breakpoints))
          'done))
      (define (cancel-breakpoint label offset)
        (set! breakpoints
              (remove (lambda (breakpoint)
                        (and (eq? (breakpoint-label breakpoint) label)
                             (= (breakpoint-offset breakpoint) offset)))
                      breakpoints))
        'done)
      (define (stop-at breakpoint)
        (display (list 'breakpoint
                       (breakpoint-label breakpoint)
                       (breakpoint-offset breakpoint)))
        (newline)
        'breakpoint)
      (define (execute resuming?)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((and (not resuming?) (assq (car insts) breakpoints))
                 => stop-at)
                (else
                 ((instruction-execution-proc (car insts)))
                 (execute #f)))))
      (define (dispatch message)
        (case message
          ((start)
           (set-contents! pc instruction-sequence)
           (execute #f))
          ((proceed) (execute #t))
          ((install-instruction-sequence)
           (lambda (seq) (set! instruction-sequence seq)))
          ((install-labels)
           (lambda (machine-labels) (set! labels machine-labels)))
          ((allocate-register) allocate-register)
          ((get-register) lookup-register)
          ((install-operations)
           (lambda (ops) (set! operations (append operations ops))))
          ((stack) stack)
          ((operations) operations)
          ((set-breakpoint) set-breakpoint)
          ((cancel-breakpoint) cancel-breakpoint)
          ((cancel-all-breakpoints) (set! breakpoints '()) 'done)
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (set-breakpoint machine label offset)
  ((machine 'set-breakpoint) label offset))

(define (cancel-breakpoint machine label offset)
  ((machine 'cancel-breakpoint) label offset))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (proceed-machine machine)
  (machine 'proceed))

(define (result-and-output thunk)
  (let* ((result #f)
         (output (with-output-to-string
                   (lambda () (set! result (thunk))))))
    (list result output)))

(define (registers machine . names)
  (map (lambda (name) (get-register-contents machine name)) names))

(define gcd-machine (make-gcd-machine))
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(set-breakpoint gcd-machine 'test-b 4)

(check (result-and-output (lambda () (start gcd-machine)))
       => '(breakpoint "(breakpoint test-b 4)\n"))
(check (registers gcd-machine 'a 'b 't) => '(206 40 6))

(check (result-and-output (lambda () (proceed-machine gcd-machine)))
       => '(breakpoint "(breakpoint test-b 4)\n"))
(check (registers gcd-machine 'a 'b 't) => '(40 6 4))

(cancel-breakpoint gcd-machine 'test-b 4)
(check (result-and-output (lambda () (proceed-machine gcd-machine)))
       => '(done ""))
(check (registers gcd-machine 'a) => '(2))

(define factorial-machine (make-factorial-machine))
(set-register-contents! factorial-machine 'n 3)
(set-breakpoint factorial-machine 'base-case 1)
(set-breakpoint factorial-machine 'after-fact 3)

(check (result-and-output (lambda () (start factorial-machine)))
       => '(breakpoint "(breakpoint base-case 1)\n"))
(check (registers factorial-machine 'n) => '(1))
(check (stack-statistics factorial-machine)
       => '((total-pushes . 4) (maximum-depth . 4)))

(check (result-and-output (lambda () (proceed-machine factorial-machine)))
       => '(breakpoint "(breakpoint after-fact 3)\n"))
(check (registers factorial-machine 'n 'val) => '(2 1))

(check (result-and-output (lambda () (proceed-machine factorial-machine)))
       => '(breakpoint "(breakpoint after-fact 3)\n"))
(check (registers factorial-machine 'n 'val) => '(3 2))

(cancel-all-breakpoints factorial-machine)
(check (result-and-output (lambda () (proceed-machine factorial-machine)))
       => '(done ""))
(check (registers factorial-machine 'val) => '(6))

(check-error (set-breakpoint factorial-machine 'no-such-label 1))
(check-error (set-breakpoint factorial-machine 'fact-loop 0))
(check-error (set-breakpoint factorial-machine 'fact-done 1))
