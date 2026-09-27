(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "3.75.scm" (current-load-pathname)))

(define (average a b) (/ (+ a b) 2))

;; As in 3.75 the value before the first one is taken to be 0.
(define (smooth s)
  (stream-map average s (cons-stream 0 s)))

(define (modular-smoothed-zero-crossings sense-data)
  (make-zero-crossings (smooth sense-data)))

(check (stream->list (smooth (list->stream '(2 4 -2 0)))) => '(1 3 1 -1))
(check (stream->list (modular-smoothed-zero-crossings signal))
       => '(0 0 -1 0))
(check (stream->list (modular-smoothed-zero-crossings noisy))
       => (stream->list (smoothed-zero-crossings noisy 0 0)))
(check (stream->list (modular-smoothed-zero-crossings sense-data))
       => (stream->list (smoothed-zero-crossings sense-data 0 0)))
