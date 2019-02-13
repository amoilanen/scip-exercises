(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (make-center-percent c p)
  (let ((w (* (/ p 100) c)))
    (make-interval (- c w) (+ c w))))

(define interval1 )
(define interval2 (make-interval 4 5))

(newline)
; (8 . 12)
(display (make-center-percent 10 20))