(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "3.81.scm" (current-load-pathname)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (map-successive-pairs f s)
  (cons-stream (f (stream-car s) (stream-car (stream-cdr s)))
               (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define generate-requests (cons-stream 'generate generate-requests))

;; Uniformly distributed in [0, 1).
(define random-fractions
  (stream-map (lambda (x) (exact->inexact (/ x random-modulus)))
              (random-numbers generate-requests)))

(define (estimate-integral p x1 x2 y1 y2)
  (define (in-range fraction low high)
    (+ low (* fraction (- high low))))
  (define experiments
    (map-successive-pairs (lambda (u v)
                            (p (in-range u x1 x2) (in-range v y1 y2)))
                          random-fractions))
  (scale-stream (monte-carlo experiments 0 0)
                (* (- x2 x1) (- y2 y1))))

(define (inside-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(define pi-estimates (estimate-integral inside-unit-circle? -1. 1. -1. 1.))

(check (every (lambda (x) (and (>= x 0) (< x 1)))
              (stream-head random-fractions 1000))
       => #t)
(check (stream-head (estimate-integral (lambda (x y) #t) 2 4 0 3) 3)
       => '(6 6 6))
(check (stream-ref pi-estimates 20000) (=> (approx= 0.02)) 3.14159)
