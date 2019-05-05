(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

; 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect a v)
  (make-vect
   (* a (xcor-vect v))
   (* a (ycor-vect v))))

(define (neg-vect v)
  (scale-vect -1 v))

(define (sub-vect v1 v2)
  (add-vect v1 (neg-vect v2)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; Generates JavaScript code for drawing on a canvas
(define (draw-line v1 v2)
  (newline)
  (display "ctx.beginPath();")
  (display (string-append "ctx.moveTo(" (number->string (xcor-vect v1)) "," (number->string (ycor-vect v1)) ");"))
  (display (string-append "ctx.lineTo(" (number->string (xcor-vect v2)) "," (number->string (ycor-vect v2)) ");"))
  (display "ctx.stroke();"))

;<html>
;<body>
;<canvas width="500" height="500"></canvas>
;<script>
;var canvas = document.querySelector('canvas');
;var ctx = canvas.getContext('2d');
;//TODO: Insert the generated code here
;</script>
;</body>
;</html>

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (below painter1 painter2)
  (rotate-180 (beside painter1 painter2)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

; 2.49 d. Simplified version of the wave pattern, the real one is similarly built, but more points
(define (wave frame)
  ((segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 0.5 0.5))
      (make-segment (make-vect 0 1) (make-vect 0.5 0.5))
      (make-segment (make-vect 0.5 1) (make-vect 0.5 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 0.5))
      (make-segment (make-vect 1 0) (make-vect 0.5 0.5)))) frame))

(define base-frame
  (make-frame
    (make-vect 0 0)
    (make-vect 500 0)
    (make-vect 0 500)))

(newline)
(newline)
(display "wave:")
(wave base-frame)

(newline)
(newline)
(display "flip-horiz wave:")
((flip-horiz wave) base-frame)

(newline)
(newline)
(display "rotate-180 wave:")
((rotate-180 wave) base-frame)

(newline)
(newline)
(display "rotate-270 wave:")
((rotate-270 wave) base-frame)

(newline)
(newline)
(display "below wave:")
((below wave wave) base-frame)