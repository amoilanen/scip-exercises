(define (pow x n)
  (if (equal? n 0)
    1
    (*
      x
      (pow x (- n 1)))))

(define (nth-root x n)
  (define (good-enough? guess)
    (<
      (/
        (abs
          (- (improve guess) guess))
        guess)
      0.001))
  (define (improve guess)
    (/
      (+
        (/ x (pow guess (- n 1)))
        (* guess (- n 1)))
      n))
  (define (nth-root-iter guess)
    (if (good-enough? guess)
      guess
      (nth-root-iter (improve guess))))
  (nth-root-iter 1.0))

(define (sqrt x)
  (nth-root x 2))

(define (cubic-root x)
  (nth-root x 3))