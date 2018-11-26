(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x)
  (+ 1 x))

(display ((compose square inc) 6))