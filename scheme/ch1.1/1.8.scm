(define (pow x n)
  (if (equal? n 0)
    1
    (*
      x
      (pow x (- n 1)))))

(define (nth-root-iter guess x n)
  (if (good-enough? guess x n)
    guess
    (nth-root-iter
      (improve guess x n)
      x
      n)))

(define (improve guess x n)
  (/
    (+
      (/ x (pow guess (- n 1)))
      (* guess (- n 1)))
    n))

(define (good-enough? guess x n)
  (<
    (/
      (abs
        (- (improve guess x n) guess))
      guess)
    0.001))

(define (nth-root x n)
  (nth-root-iter 1.0 x n))

(define (sqrt x)
  (nth-root x 2))

(define (cubic-root x)
  (nth-root x 3))