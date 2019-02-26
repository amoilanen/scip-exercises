(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (if (null? (cdr items))
      (list (car items))
      (append
        (reverse (cdr items))
        (list (car items)))))

(display (reverse (list 1 2 3 4 5)))