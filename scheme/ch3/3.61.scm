(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "3.60.scm" (current-load-pathname)))

;; For a series S with constant term 1, X = 1 - S_R X.
(define (invert-unit-series s)
  (define x
    (cons-stream 1 (scale-stream (mul-series (stream-cdr s) x) -1)))
  x)

(define zeros (cons-stream 0 zeros))

(define one-minus-x (cons-stream 1 (cons-stream -1 zeros)))

(check (stream-head (invert-unit-series one-minus-x) 5) => '(1 1 1 1 1))
(check (stream-head (invert-unit-series exp-series) 5)
       => '(1 -1 1/2 -1/6 1/24))
(check (stream-head (mul-series exp-series (invert-unit-series exp-series)) 6)
       => '(1 0 0 0 0 0))
