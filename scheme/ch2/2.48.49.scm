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

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; Simplified drawing of the line on a board, draws lines as text
(define (draw-line v1 v2)
  (newline)
  (display v1)
  (display "->")
  (display v2))

; Generates JavaScript code for drawing on a canvas
(define (draw-line v1 v2)
  (newline)
  (display "ctx.beginPath();")
  (display (string-append "ctx.moveTo(" (number->string (xcor-vect v1)) "," (number->string (ycor-vect v1)) ");"))
  (display (string-append "ctx.lineTo(" (number->string (xcor-vect v2)) "," (number->string (ycor-vect v2)) ");"))
  (display "ctx.stroke();"))

;<html>
;<body>
;<canvas width="200" height="200"></canvas>
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

; 2.49 a.
(define (painters->frame-outline frame)
  ((segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 0 1))
      (make-segment (make-vect 0 1) (make-vect 1 1))
      (make-segment (make-vect 1 1) (make-vect 1 0))
      (make-segment (make-vect 1 0) (make-vect 0 0)))) frame))

; 2.49 b.
(define (painters->frame-x frame)
  ((segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0)))) frame))

; 2.49 c.
(define (painters->frame-diamond frame)
  ((segments->painter
    (list
      (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
      (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))) frame))

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
    (make-vect 50 50)
    (make-vect 50 0)
    (make-vect 50 50)))

(newline)
(newline)
(display "frame-outline:")
(painters->frame-outline base-frame)

(newline)
(newline)
(display "frame-x:")
(painters->frame-x base-frame)

(newline)
(newline)
(display "frame-diamond:")
(painters->frame-diamond base-frame)

(newline)
(newline)
(display "wave:")
(wave base-frame)