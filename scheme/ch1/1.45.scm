(define small-delta 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) small-delta))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (identity x) x)

(define (repeated f times)
  (cond
      ((= times 0) identity)
      ((= times 1) f)
      (else
        (lambda (x)
          (f
            ((repeated f (- times 1)) x))))))

(define (nth-root x n)
  (let (
         (multi-damp (repeated average-damp (- n 2)))
         (f (lambda (y) (/ x (expt y (- n 1))))))
    (exact->inexact
      (fixed-point (multi-damp f) 1.5))))

