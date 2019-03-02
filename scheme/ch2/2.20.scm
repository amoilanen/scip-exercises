(define (filter items predicate)
  (if (null? items)
    items
    (if (predicate (car items))
      (cons (car items) (filter (cdr items) predicate))
      (filter (cdr items) predicate))))

(define (same-parity . numbers)
  (define (parity number)
    (remainder number 2))
  (if (null? numbers)
    numbers
    (let ((first-number-parity (parity (car numbers))))
    (filter
      numbers
      (lambda (x)
        (= (parity x) first-number-parity))))))

(newline)
(display (same-parity 1 2 3 4 5 6 7))
; (1 3 5 7)

(newline)
(display (same-parity 2 3 4 5 6 7))
; (2 4 6)

(newline)
(display (same-parity 1))
; (1)

(newline)
(display (same-parity))
; ()