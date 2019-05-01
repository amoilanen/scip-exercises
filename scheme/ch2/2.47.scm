(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define f1 (make-frame 1 2 3))

(newline)
(display (origin-frame f1))
(newline)
(display (edge1-frame f1))
(newline)
(display (edge2-frame f1))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define f1 (make-frame 1 2 3))

(newline)
(display (origin-frame f1))
(newline)
(display (edge1-frame f1))
(newline)
(display (edge2-frame f1))