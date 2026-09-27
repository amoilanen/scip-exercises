(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "2.93.scm" (current-load-pathname)))

(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (gcd-poly p1 p2)
  (make-poly (common-variable p1 p2 'gcd-poly)
             (gcd-terms (term-list p1) (term-list p2))))

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

(put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
(put 'greatest-common-divisor '(polynomial polynomial)
     (lambda (p1 p2) (attach-tag 'polynomial (gcd-poly p1 p2))))

(check (greatest-common-divisor 12 18) => 6)

(define a (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define b (make-polynomial 'x '((3 1) (1 -1))))

(check (greatest-common-divisor a b) => (make-polynomial 'x '((2 -1) (1 1))))
(check (greatest-common-divisor b a) => (make-polynomial 'x '((2 -1) (1 1))))
(check (greatest-common-divisor a (make-polynomial 'x '())) => a)
(check (greatest-common-divisor a (make-polynomial 'x '((2 1) (0 1))))
       => (make-polynomial 'x '((0 2))))
(check-error (greatest-common-divisor a (make-polynomial 'y '((1 1)))))
