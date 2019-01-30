(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

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

(define x (make-interval 2 3))

(newline)
(display (width-interval x)) ;1/2
(newline)
(display
    (let ((width-x (width-interval x)))
      (+ width-x width-x))) ;1
(newline)
(display (width-interval (add-interval x x))) ;1 == 1
(newline)
(display (width-interval (mul-interval x x))) ;2.5 (as width of (4 . 9))
(newline)
(display
  (let ((width-x (width-interval x)))
    (* width-x width-x))) ;0.25 != 2.5
