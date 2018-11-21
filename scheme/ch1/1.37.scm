(define (partial-cont-frac n d i k)
  (/ (n i)
     (+ (d i)
        (if (= i k) 0
            (partial-cont-frac n d (+ i 1) k)))))

(define (partial-cont-frac-iter n d i k)
  (define (step s n d i k)
    (if (= i 0) s
        (step (/ (n i) (+ (d i) s)) n d (- i 1) k)))
  (step 0 n d k k))

(define (cont-frac n d k)
  (partial-cont-frac-iter n d 1 k))

(define phi
  (/ 1
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                100)))