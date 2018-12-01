(define good-enough-delta 0.00001)

(define (good-enough? improve guess)
  (<
    (/
      (abs
        (- (improve guess) guess))
      guess)
    good-enough-delta))

(define (iterative-improve good-enough? improve)
  (define (iteration guess)
    (if (good-enough? improve guess)
      guess
      (iteration (improve guess))))
  (lambda (first-guess)
    (iteration first-guess)))

; sqrt defined via iterative-improve
(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (sqrt-improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? sqrt-improve) 1.0))

(newline)
(display (sqrt 16))

; fixed-point defined via iterative-improve
(define (fixed-point f first-guess)
  (define (fixed-point-improve guess)
    (f guess))
  ((iterative-improve good-enough? fixed-point-improve) first-guess))

(define fi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(newline)
(display fi)