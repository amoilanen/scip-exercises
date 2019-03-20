(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (if (and (not (pair? t)) (not (null? t)))
    1
    (accumulate
      (lambda (cur acc) (+ cur acc))
      0
      (map count-leaves t))))

(define x (cons (list 1 2) (list 3 4)))

(newline)
(display (count-leaves (list x x)))

