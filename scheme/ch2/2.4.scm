(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define p (cons 1 2))
(newline)
(display (car p))
(newline)
(display (cdr p))