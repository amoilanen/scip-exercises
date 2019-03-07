(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe tree)
  (cond
    ((null? tree) ())
    ((not (pair? tree)) (list tree))
    (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list 3 4 (list 5 6))))

(newline)
(display (fringe x)) ; (1 2 3 4)

(newline)
(display (fringe (list x x))) ; (1 2 3 4 1 2 3 4)

(newline)
(display (fringe y)) ; (1 2 3 4 5 6)