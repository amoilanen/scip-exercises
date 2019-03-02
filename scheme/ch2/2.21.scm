(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))

(newline)
(display (square-list (list 1 2 3)))

(define (square-list items)
  (map square items))

(newline)
(display (square-list (list 1 2 3)))
