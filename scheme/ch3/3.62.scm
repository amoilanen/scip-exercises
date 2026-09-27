(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "3.61.scm" (current-load-pathname)))

(define (div-series numerator denominator)
  (let ((constant (stream-car denominator)))
    (if (= constant 0)
        (error "Denominator has a zero constant term -- DIV-SERIES"
               denominator)
        (scale-stream
         (mul-series numerator
                     (invert-unit-series
                      (scale-stream denominator (/ 1 constant))))
         (/ 1 constant)))))

(define tangent-series (div-series sine-series cosine-series))

(check (stream-head tangent-series 8) => '(0 1 0 1/3 0 2/15 0 17/315))
(check (stream-head (div-series ones (scale-stream one-minus-x 2)) 4)
       => '(1/2 1 3/2 2))
(check (stream-head (div-series exp-series exp-series) 5) => '(1 0 0 0 0))
(check-error (div-series cosine-series sine-series))
