(define (mult a b)
  (define (mult-iter r a b)
    (cond ((= b 0) r)
          ((even? b) (mult-iter r (double a) (halve b)))
          (else (mult-iter (+ r a) a (- b 1)))))
  (mult-iter 0 a b))

(define (double x)
  (arithmetic-shift x 1))

(define (halve x)
  (arithmetic-shift x -1))

(define (even? n)
  (= (remainder n 2) 0))