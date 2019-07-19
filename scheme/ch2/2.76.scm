(define pi 3.14159)

(define get 2d-get)
(define put 2d-put!)

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (generic-dispatch)
  (define (perimeter-rectangle r) (* 2 (+ (car r) (cdr r))))

  (define (make-rectangle x y)
    (attach-tag 'rectangle
              (cons x y)))

  (define (perimeter-circle r) (* 2 pi r))
  (define (make-circle r)
   (attach-tag 'circle r))

  (define (rectangle? r)
    (eq? (type-tag r) 'rectangle))
  (define (circle? c)
    (eq? (type-tag c) 'circle))

  (define (perimeter z)
    (cond ((rectangle? z) (perimeter-rectangle (contents z)))
          ((circle? z) (perimeter-circle (contents z)))
          (else (error "Unknown type -- PERIMETER" z))))

  (define rect (make-rectangle 3 4))
  (define circ (make-circle 1))

  (newline)
  (display (perimeter rect))
  (newline)
  (display (perimeter circ))
'done)

(generic-dispatch)