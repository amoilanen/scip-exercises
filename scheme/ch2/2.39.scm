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

(define (reverse sequence)
  (fold-right (lambda (cur acc) (append acc (list cur))) () sequence))

(newline)
(display (reverse (list 1 2 3 4 5)))

(define (reverse sequence)
  (fold-left (lambda (acc cur) (cons cur acc)) () sequence))

(newline)
(display (reverse (list 1 2 3 4 5)))

