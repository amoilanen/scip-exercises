(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(newline)
(display (exact->inexact (fold-right / 1 (list 1 2 3)))) ; (1 / (2 / (3 / 1))) = 1.5

(newline)
(display (exact->inexact (fold-left / 1 (list 1 2 3)))) ; 1 / (1 / (2 / 3)) = 0.16667

(newline)
(display (fold-right list () (list 1 2 3))) ; (1 (2 (3)))

(newline)
(display (fold-left list () (list 1 2 3))) ; (((1) 2) 3)

; fold-left and fold-right are the same if the operation op is associatice, for example +

(newline)
(display (fold-right + 0 (list 1 2 3))) ; 6

(newline)
(display (fold-left + 0 (list 1 2 3))) ; 6