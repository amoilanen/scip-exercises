(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

(define (partial-sums s)
  (define sums
    (cons-stream (stream-car s) (add-streams (stream-cdr s) sums)))
  sums)

(check (stream-head (partial-sums integers) 5) => '(1 3 6 10 15))
(check (stream-head (partial-sums ones) 4) => '(1 2 3 4))
(check (stream->list (partial-sums (list->stream '(5 -2 4))))
       => '(5 3 7))
