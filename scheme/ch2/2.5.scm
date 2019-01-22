(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (divides? n m)
  (integer? (/ n m)))

(define (find-expt-of-divisor n d)
  (define (try-expt-of-divisor-iter n d e)
    (if (divides? n (expt d e))
        (try-expt-of-divisor-iter n d (+ e 1))
        (- e 1)))
  (try-expt-of-divisor-iter n d 1))

(define (car z)
  (find-expt-of-divisor z 2))

(define (cdr z)
  (find-expt-of-divisor z 3))

(define p (cons 1 2))
(newline)
(display (car p))
(newline)
(display (cdr p))