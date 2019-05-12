

(define (equal? x y)
  (or (eq? x y)
      (and
        (pair? x)
        (pair? y)
        (equal?
          (car x) (car y))
        (equal?
          (cdr x) (cdr y)))))

; true
(display (equal? '(this is a list) '(this is a list)))

; false
(display (equal? '(this is a list) '(this (is a) list)))