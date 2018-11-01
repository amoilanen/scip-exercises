(define (sqrt-iter guess x stop-check)
  (if (stop-check guess x)
    guess
    (sqrt-iter
      (improve guess x)
      x
      stop-check)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (better-good-enough? guess x)
  (<
    (/
      (abs
        (- (improve guess x) guess))
      guess)
    0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x good-enough?))

(define (better-sqrt x)
  (sqrt-iter 1.0 x better-good-enough?))