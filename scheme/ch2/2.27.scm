(define (reverse items)
  (define (reverse-iter items acc)
    (cond
      ((null? items) acc)
      ((not (pair? items)) items)
      (else (reverse-iter (cdr items) (cons (car items) acc)))))
  (reverse-iter items ()))

(define (deep-reverse tree)
  (define (deep-reverse-iter items acc)
    (cond
      ((null? items) acc)
      ((not (pair? items)) items)
      (else (deep-reverse-iter
        (cdr items)
        (cons (deep-reverse (car items)) acc)))))
  (deep-reverse-iter tree ()))

(define x (list (list 1 2) (list 3 4 (list 5 6))))

(newline)
(display (reverse x))

(newline)
(display (deep-reverse x))