(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))

(define (double x)
  (arithmetic-shift x 1))

(define (halve x)
  (arithmetic-shift x -1))

(define (even? n)
  (= (remainder n 2) 0))