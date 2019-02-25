(define (last-pair items)
  (define (last-pair-iter items pair)
    (if (null? items)
        (list pair)
        (last-pair-iter (cdr items) (car items))))
  (last-pair-iter items 'nil))

; Alternative non-iterative implementation
(define (last-pair-alt items)
  (if (null? (cdr items))
    (list (car items))
    (last-pair-alt (cdr items))))

(newline)
(display
  (last-pair (list 23 72 149 34)))
(newline)
(display
  (last-pair-alt (list 23 72 149 34)))
