(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing? #f))
    (define (set value)
      (if tracing?
          (begin (display name)
                 (display ": ")
                 (write contents)
                 (display " -> ")
                 (write value)
                 (newline)))
      (set! contents value))
    (lambda (message)
      (case message
        ((get) contents)
        ((set) set)
        ((trace-on) (set! tracing? #t) 'done)
        ((trace-off) (set! tracing? #f) 'done)
        (else (error "Unknown request -- REGISTER" message))))))

(define (trace-register-on machine register-name)
  ((get-register machine register-name) 'trace-on))

(define (trace-register-off machine register-name)
  ((get-register machine register-name) 'trace-off))

(define gcd-machine (make-gcd-machine))

(define (gcd-trace a b)
  (with-output-to-string
    (lambda ()
      (run-machine gcd-machine (list (list 'a a) (list 'b b)) 'a))))

(trace-register-on gcd-machine 'a)
(check (gcd-trace 206 40)
       => (string-append "a: *unassigned* -> 206\n"
                         "a: 206 -> 40\n"
                         "a: 40 -> 6\n"
                         "a: 6 -> 4\n"
                         "a: 4 -> 2\n"))

(trace-register-on gcd-machine 't)
(trace-register-off gcd-machine 'a)
(check (gcd-trace 12 8)
       => (string-append "t: 0 -> 4\n"
                         "t: 4 -> 0\n"))

(trace-register-off gcd-machine 't)
(check (gcd-trace 12 8) => "")
(check (get-register-contents gcd-machine 'a) => 4)
