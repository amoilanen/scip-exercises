(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

(define (integrate-series coefficients)
  (stream-map / coefficients integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(check (stream-head (integrate-series ones) 4) => '(1 1/2 1/3 1/4))
(check (stream-head exp-series 5) => '(1 1 1/2 1/6 1/24))
(check (stream-head cosine-series 6) => '(1 0 -1/2 0 1/24 0))
(check (stream-head sine-series 6) => '(0 1 0 -1/6 0 1/120))
