(load "lib/check.scm")

(define (rand-update x)
  (modulo (+ (* 1103515245 x) 12345) 2147483648))

(define random-init 2026)

(define rand
  (let ((x random-init))
    (lambda (message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))
            (else (error "Unknown request -- RAND" message))))))

(define (take-random n)
  (if (= n 0)
      '()
      (let ((value (rand 'generate)))
        (cons value (take-random (- n 1))))))

((rand 'reset) 7)
(define first-run (take-random 5))
((rand 'reset) 7)
(check (take-random 5) => first-run)
(check (car first-run) => (rand-update 7))
(check (cadr first-run) => (rand-update (rand-update 7)))

((rand 'reset) 8)
(check (equal? (take-random 5) first-run) => #f)
(check-error (rand 'shuffle))
