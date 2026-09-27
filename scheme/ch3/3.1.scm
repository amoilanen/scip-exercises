(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))

(define a (make-accumulator 5))
(check (a 10) => 15)
(check (a 10) => 25)

(define b (make-accumulator 0))
(check (b 1) => 1)
(check (a 0) => 25)
