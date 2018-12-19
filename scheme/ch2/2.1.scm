(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sign x)
  (cond ((= 0 x) 0)
        ((> 0 x) 1)
        ((< 0 x) -1)))

(define (combined-sign x y)
  (* (sign x) (sign y)))

(define (make-rat n d)
  (let ((g (gcd n d)))
       (let ((n_norm (/ n g))
             (d_norm (/ d g)))
            (cons
              (* (sign n) (sign d) (abs n_norm))
              (abs d_norm)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (cond ((> (denom x) 1)
         ((display (denom x))
          (display "/")
          (display (numer x))))
        ((= (denom x) 1)
         (display (numer x)))
        (else (display "NaN"))))

;-2/3
(print-rat (make-rat 2 -3))
;2
(print-rat (make-rat -8 -4))
;3/5
(print-rat (make-rat 9 15))
;0
(print-rat (make-rat 0 15))
;NaN
(print-rat (make-rat 9 0))