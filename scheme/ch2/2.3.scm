(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;;; One way to define a rectangle: left-bottom and right-top points
(define (make-rectangle left-bottom right-top)
  (cons left-bottom right-top))

(define (rect-side rect coord-accessor)
  (abs (- (coord-accessor (car rect)) (coord-accessor (cdr rect)))))

(define (rect-width rect)
  (rect-side rect x-point))

(define (rect-height rect)
  (rect-side rect y-point))

(define example-rect (make-rectangle (make-point 1 1) (make-point 3 3)))
;;;

;;; Another way to define a rectangle: left-bottom point, width and height
(define (make-rectangle left-bottom width height)
  (cons (cons width height) left-bottom))

(define (rect-width rect)
  (car (car rect)))

(define (rect-height rect)
  (cdr (car rect)))

(define example-rect (make-rectangle (make-point 1 1) 2 2))
;;;

(define (rect-perimeter rect)
  (let ((width (rect-width rect))
        (height (rect-height rect)))
       (* 2 (+ width height))))

(define (rect-area rect)
  (let ((width (rect-width rect))
        (height (rect-height rect)))
       (* width height)))

; 8
(newline)
(display (rect-perimeter example-rect))
; 4
(newline)
(display (rect-area example-rect))