(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (mul-interval x y)
  (let ((lower-x (lower-bound x))
        (upper-x (upper-bound x))
        (lower-y (lower-bound y))
        (upper-y (upper-bound y)))
    (cond ((and (< 0 lower-x) (< 0 lower-y)) (make-interval (* lower-x lower-y) (* upper-x upper-y)))
          ((and (> 0 upper-x) (> 0 upper-y)) (make-interval (* upper-x upper-y) (* lower-x lower-y)))
          ((and (< 0 lower-x) (> 0 upper-y)) (make-interval (* upper-x lower-y) (* lower-x upper-y)))
          ((and (> 0 upper-x) (< 0 lower-y)) (make-interval (* lower-x upper-y) (* upper-x lower-y)))
          ((and (>= 0 lower-x) (<= 0 upper-x) (< 0 lower-y)) (make-interval (* lower-x upper-y) (* upper-x upper-y)))
          ((and (< 0 lower-x) (>= 0 lower-y) (<= 0 upper-y)) (make-interval (* upper-x lower-y) (* upper-x upper-y)))
          ((and (>= 0 lower-x) (<= 0 upper-x) (> 0 upper-y)) (make-interval (* upper-x lower-y) (* lower-x lower-y)))
          ((and (> 0 upper-x) (>= 0 lower-y) (<= 0 upper-y)) (make-interval (* lower-x upper-y) (* lower-x lower-y)))
          ((and (>= 0 lower-x) (<= 0 upper-x) (>= 0 lower-y) (<= 0 upper-y))
            (make-interval
                (min (* upper-x lower-y) (* lower-x upper-y))
                (max (* lower-x lower-y) (* upper-x upper-y))))
    )))

(define x (make-interval 2 4))
(define y (make-interval 3 5))

(newline)
; (6 . 20)
(display (mul-interval x y))