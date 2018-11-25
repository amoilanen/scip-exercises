(define (partial-cont-frac-iter n d i k)
  (define (step s n d i k)
    (if (= i 0) s
        (step (/ (n i) (+ (d i) s)) n d (- i 1) k)))
  (step 0 n d k k))

(define (cont-frac n d k)
  (partial-cont-frac-iter n d 1 k))

(define (d i)
  (+ (* 2 (- i 1)) 1))

(define (n x)
  (lambda (i)
    (if (= i 1) x
        (* -1 x x))))

(define (tan-cf x k)
  (cont-frac (n x) d k))

(newline)
(display (tan 1))
(newline)
(display (exact->inexact (tan-cf 1 100)))