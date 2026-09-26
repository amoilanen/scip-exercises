(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (partial-sums s)
  (define sums
    (cons-stream (stream-car s) (add-streams (stream-cdr s) sums)))
  sums)

(check (stream-head (partial-sums integers) 5) => '(1 3 6 10 15))
(check (stream-head (partial-sums ones) 4) => '(1 2 3 4))
(check (stream->list (partial-sums (list->stream '(5 -2 4))))
       => '(5 3 7))
