(load "lib/check.scm")
(load "ch2/2.94.scm")

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

;; The result is P1 multiplied by 1458/169, not P1.  Dividing Q1 by Q2
;; divides the leading coefficients 11 and 13, so the quotient
;; 11/13 x - 55/169 and the remainder 1458/169 (x^2 - 2x + 1) have
;; non-integer coefficients.  That remainder already divides Q2, so it is
;; returned.  Over the rationals a GCD is only determined up to a constant
;; factor, and Euclid's algorithm returns whichever multiple it runs into.

(check (div-terms (term-list (contents q1)) (term-list (contents q2)))
       => '(((1 11/13) (0 -55/169))
            ((2 1458/169) (1 -2916/169) (0 1458/169))))

(check (greatest-common-divisor q1 q2)
       => (make-polynomial 'x '((2 1458/169) (1 -2916/169) (0 1458/169))))
(check (equ? (greatest-common-divisor q1 q2)
             (mul (make-polynomial 'x '((0 1458/169))) p1))
       => #t)
