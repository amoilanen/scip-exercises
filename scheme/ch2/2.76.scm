(define pi 3.14159)

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
  (define (rectangle? r)
    (eq? (type-tag r) 'rectangle))

  (define (perimeter-circle r) (* 2 pi r))
  (define (make-circle r)
   (attach-tag 'circle r))
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

(define (data-directed-dispatch)
  (define get 2d-get)
  (define put 2d-put!)
  (define (apply-generic op arg)
      (let ((proc (get op (type-tag arg))))
        (if proc
            (proc (contents arg))
            (error
              "No method for these types -- APPLY-GENERIC"
              (list op (type-tag arg))))))

  (define (perimeter-rectangle r) (* 2 (+ (car r) (cdr r))))
  (define (make-rectangle x y)
    (attach-tag 'rectangle
              (cons x y)))
  (put 'perimeter 'rectangle perimeter-rectangle)

  (define (perimeter-circle r) (* 2 pi r))
  (define (make-circle r)
   (attach-tag 'circle r))
  (put 'perimeter 'circle perimeter-circle)

  (define (perimeter z)
    (apply-generic 'perimeter z))

  (define rect (make-rectangle 3 4))
  (define circ (make-circle 1))

  (newline)
  (display (perimeter rect))
  (newline)
  (display (perimeter circ))
'done)

(define (message-passing-dispatch)
  (define (make-rectangle x y)
    (define (dispatch op)
      (cond ((eq? op 'perimeter) (* 2 (+ x y)))
            (else
              (error "Unknown op -- rectangle" op))))
    dispatch)

  (define (make-circle r)
    (define (dispatch op)
      (cond ((eq? op 'perimeter) (* 2 pi r))
            (else
              (error "Unknown op -- circle" op))))
    dispatch)

  (define (apply-generic op arg) (arg op))
  (define (perimeter z)
    (apply-generic 'perimeter z))

  (define rect (make-rectangle 3 4))
  (define circ (make-circle 1))

  (newline)
  (display (perimeter rect))
  (newline)
  (display (perimeter circ))
'done)

(generic-dispatch)
(data-directed-dispatch)
(message-passing-dispatch)