(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2
            (f (- n 2)))
         (* 3
            (f (- n 3))))))

(define (fi n)
  (define (f-iter x y z n)
    (if (= n 0)
      z
      (f-iter (+ x
                 (* 2 y)
                 (* 3 z))
              x
              y
              (- n 1))))
  (f-iter 2 1 0 n))