(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (
            (display "Found")
            next
          )
          (try next))))
  (try first-guess))

(define (average-damp f) (lambda (x) (/ (+ x (f x)) 2)))

(define f (lambda (x) (/ (log 1000) (log x))))

(define solution (fixed-point f 2.0))
(define solution-damp (fixed-point (average-damp f) 2.0))