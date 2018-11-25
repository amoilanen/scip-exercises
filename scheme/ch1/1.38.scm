(define (d j)
  (let (
         (i (+ j 1))
         (r (remainder (+ j 1) 3))
       )
    (cond ((or (= 1 r) (= 2 r)) 1)
          ((= 0 r) (* (/ i 3) 2)))))

(define (d-up-to k)
  (define (d-from-to i k)
    (if (> i k) '()
        (cons (d i) (d-from-to (+ 1 i) k))))
  (d-from-to 1 k))

(define (partial-cont-frac-iter n d i k)
  (define (step s n d i k)
    (if (= i 0) s
        (step (/ (n i) (+ (d i) s)) n d (- i 1) k)))
  (step 0 n d k k))

(define (cont-frac n d k)
  (partial-cont-frac-iter n d 1 k))

(define e
  (+ 2
     (cont-frac (lambda (i) 1.0)
                d
                100)))

(newline)
(display (d-up-to 11))
(newline)
(display e)