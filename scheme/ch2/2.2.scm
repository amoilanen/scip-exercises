(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((x-start (x-point (start-segment segment)))
         (y-start (y-point (start-segment segment)))
         (x-end (x-point (end-segment segment)))
         (y-end (y-point (end-segment segment))))
    (make-point (average x-start x-end) (average y-start y-end))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Computing and displaying the midpoint of a segment
(define point1 (make-point 1 1))
(define point2 (make-point 3 3))
(define segment1 (make-segment point1 point2))

(print-point (midpoint-segment segment1))