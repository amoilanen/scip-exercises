(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand))
                   (+ (* dt (stream-car integrand)) initial-value)
                   dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(check (stream->list (integral (delay (list->stream '(1 2 3))) 10 0.5))
       => '(10 10.5 11.5 13.))
(check (stream-head (integral (delay ones) 0 1) 4) => '(0 1 2 3))
(check (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
       (=> (approx= 1e-6))
       2.716924)
