(load "lib/check.scm")
(load "ch2/2.95.scm")

(define (scale-terms c terms)
  (mul-term-by-all-terms (make-term 0 c) terms))

(define (leading-order terms)
  (order (first-term terms)))

(define (leading-coeff terms)
  (coeff (first-term terms)))

;;; a.

;; When a is of lower order than b, a is its own (pseudo)remainder.
(define (pseudoremainder-terms a b)
  (if (or (empty-termlist? a) (< (leading-order a) (leading-order b)))
      a
      (let ((factor (expt (leading-coeff b)
                          (+ 1 (- (leading-order a) (leading-order b))))))
        (remainder-terms (scale-terms factor a) b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))

(check (pseudoremainder-terms (term-list (contents q1))
                              (term-list (contents q2)))
       => '((2 1458) (1 -2916) (0 1458)))
(check (greatest-common-divisor q1 q2)
       => (make-polynomial 'x '((2 1458) (1 -2916) (0 1458))))

;;; b.

(define (coefficients-gcd terms)
  (if (empty-termlist? terms)
      0
      (gcd (leading-coeff terms) (coefficients-gcd (rest-terms terms)))))

(define (remove-common-factor terms)
  (if (empty-termlist? terms)
      terms
      (scale-terms (/ 1 (coefficients-gcd terms)) terms)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      (remove-common-factor a)
      (gcd-terms b (pseudoremainder-terms a b))))

(check (greatest-common-divisor q1 q2) => p1)
(check (greatest-common-divisor (mul q1 (make-polynomial 'x '((0 6))))
                                (mul q2 (make-polynomial 'x '((0 4)))))
       => p1)
; a and b are the polynomials of exercise 2.94.
(check (greatest-common-divisor a b) => (make-polynomial 'x '((2 -1) (1 1))))
(check (greatest-common-divisor p2 p3) => (make-polynomial 'x '((0 1))))
(check (greatest-common-divisor p1 (make-polynomial 'x '())) => p1)
