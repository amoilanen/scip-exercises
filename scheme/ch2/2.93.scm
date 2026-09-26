(load "lib/check.scm")
;; Exercises 2.94-2.97 build on this file and need polynomial division too.
(load "ch2/2.91.scm")

(define (make-rat n d) (cons n d))

(define (add-rat x y)
  (make-rat (add (mul (numer x) (denom y)) (mul (numer y) (denom x)))
            (mul (denom x) (denom y))))

(define (sub-rat x y)
  (add-rat x (negate-rat y)))

(define (mul-rat x y)
  (make-rat (mul (numer x) (numer y))
            (mul (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (mul (numer x) (denom y))
            (mul (denom x) (numer y))))

(define (negate-rat x)
  (make-rat (negate (numer x)) (denom x)))

(define (equ-rat? x y)
  (equ? (mul (numer x) (denom y)) (mul (numer y) (denom x))))

(define (zero-rat? x)
  (=zero? (numer x)))

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

(check rf => (cons 'rational (cons p2 p1)))
(check (add rf rf)
       => (make-rational (make-polynomial 'x '((5 2) (3 2) (2 2) (0 2)))
                         (make-polynomial 'x '((4 1) (2 2) (0 1)))))
(check (equ? (add rf rf) (mul (make-rational p2 p1)
                              (make-rational (make-polynomial 'x '((0 2)))
                                             (make-polynomial 'x '((0 1))))))
       => #t)
(check (=zero? (sub rf rf)) => #t)
(check (div rf rf) => (make-rational (mul p2 p1) (mul p1 p2)))

(check (add (make-rational 1 2) (make-rational 1 2)) => '(rational 4 . 4))
(check (equ? (make-rational 2 4) (make-rational 1 2)) => #t)
