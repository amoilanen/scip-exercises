(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (make-center-percent c p)
  (let ((w (* (/ p 100) c)))
    (make-interval (- c w) (+ c w))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((upper-y (/ 1.0 (upper-bound y))) (lower-y (/ 1.0 (lower-bound y))))
    (if (<= (* upper-y lower-y) 0)
        (error "Dividing by an interval that spans zero")
        (mul-interval x
                      (make-interval lower-y upper-y)))))

(define (display-interval x)
  (display "(")
  (display (exact->inexact (lower-bound x)))
  (display ",")
  (display (exact->inexact (upper-bound x)))
  (display ")"))

; 2.13 
; boundaries of a product of two intervals (c1 p1) (c2 p2)
; (c1 +- (p1 * c1 / 100)) * (c2 +- (p2 * c2 / 100)) =
; (c1 * c2) +- (p1 * c1 / 100) * c2 +- (p2 * c2 / 100) * c1 +- (p1 * c1 / 100) (p2 * c2 / 100)) =
; as p1 * p2 is very small
; (c1 * c2) +- (p1 * c1 / 100) * c2 +- (p2 * c2 / 100) * c1 =
; (c1 * c2) +- ((p1 + p2) * c1 * c2 / 100)
; or the error margins get added p1 + p2 for the product of the intervals

; 2.14 Results depend on the way of computing
(newline)
(define interval-1 (make-center-percent 10 5)) ; (9.5, 10.5)
(define (squared-1 x)
  (mul-interval x x))
(define (squared-2 x)
  (div-interval (mul-interval x (mul-interval x x)) x))

; (90.25, 110.25)
(display-interval (squared-1 interval-1))
(newline)
; (81.6547619047619, 121.85526315789473)
(display-interval (squared-2 interval-1))