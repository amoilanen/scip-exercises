(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect a v)
  (make-vect
   (* a (xcor-vect v))
   (* a (ycor-vect v))))

(define (neg-vect v)
  (scale-vect -1 v))

(define (sub-vect v1 v2)
  (add-vect v1 (neg-vect v2)))

(newline)
(define v1 (make-vect 1 2))
(define v2 (make-vect 2 3))
(display (add-vect v1 v2)); (3 . 5)

(newline)
(display (sub-vect v1 v2)); (-1 . -1)

(newline)
(display (scale-vect 2 v1)); (2 . 4)