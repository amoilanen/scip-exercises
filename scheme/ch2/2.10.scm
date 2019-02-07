(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

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

(define x (make-interval 8 16))
(define y (make-interval 2 4))
(define z (make-interval -2 2))

(newline)
; (2 . 8)
(display (div-interval x y))
(newline)
; "Dividing by an interval that spans zero"
(display (div-interval x z))