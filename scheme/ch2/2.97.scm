(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "2.96.scm" (current-load-pathname)))

;;; a.

(define (quotient-terms a b)
  (car (div-terms a b)))

(define (term-list-order terms)
  (if (empty-termlist? terms) 0 (leading-order terms)))

;; The common factor takes the sign of the denominator's leading coefficient,
;; so reduced denominators have a positive leading coefficient.
(define (reduce-terms n d)
  (let* ((g (gcd-terms n d))
         (factor (expt (leading-coeff g)
                       (+ 1 (- (max (term-list-order n) (term-list-order d))
                               (leading-order g)))))
         (nn (quotient-terms (scale-terms factor n) g))
         (dd (quotient-terms (scale-terms factor d) g))
         (common (* (gcd (coefficients-gcd nn) (coefficients-gcd dd))
                    (if (negative? (leading-coeff dd)) -1 1))))
    (list (scale-terms (/ 1 common) nn)
          (scale-terms (/ 1 common) dd))))

(define (reduce-poly p1 p2)
  (let ((var (common-variable p1 p2 'reduce-poly)))
    (map (lambda (terms) (make-poly var terms))
         (reduce-terms (term-list p1) (term-list p2)))))

(check (reduce-poly (contents (mul p1 p2)) (contents (mul p1 p3)))
       => (list (contents p2) (contents p3)))

;;; b.

(define (reduce-integers n d)
  (let ((g (if (negative? d) (- (gcd n d)) (gcd n d))))
    (list (/ n g) (/ d g))))

(define (reduce n d)
  (apply-generic 'reduce n d))

(put 'reduce '(scheme-number scheme-number) reduce-integers)
(put 'reduce '(polynomial polynomial)
     (lambda (p1 p2)
       (map (lambda (p) (attach-tag 'polynomial p))
            (reduce-poly p1 p2))))

(define (make-rat n d)
  (let ((reduced (reduce n d)))
    (cons (car reduced) (cadr reduced))))

(check (reduce 6 -4) => '(-3 2))
(check (make-rational 6 9) => '(rational 2 . 3))
(check (add (make-rational 1 2) (make-rational 1 2)) => '(rational 1 . 1))

(check (reduce (make-polynomial 'x '((2 1) (0 -1)))
               (make-polynomial 'x '((2 1) (1 2) (0 1))))
       => (list (make-polynomial 'x '((1 1) (0 -1)))
                (make-polynomial 'x '((1 1) (0 1)))))

(define rf1 (make-rational (make-polynomial 'x '((1 1) (0 1)))
                           (make-polynomial 'x '((3 1) (0 -1)))))
(define rf2 (make-rational (make-polynomial 'x '((1 1)))
                           (make-polynomial 'x '((2 1) (0 -1)))))

(define (rational-function n d)
  (attach-tag 'rational (cons (make-polynomial 'x n) (make-polynomial 'x d))))

(check (add rf1 rf2)
       => (rational-function '((3 1) (2 2) (1 3) (0 1))
                             '((4 1) (3 1) (1 -1) (0 -1))))
(check (sub rf1 rf1) => (rational-function '() '((0 1))))
(check (mul rf2 (make-rational (make-polynomial 'x '((1 -2) (0 2)))
                               (make-polynomial 'x '((2 4)))))
       => (rational-function '((0 -1)) '((2 2) (1 2))))
