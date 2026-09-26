(load "lib/check.scm")
(load "ch3/lib/optional-memoization.scm")

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

;; Runs the exercise's sequence of expressions and returns the value of sum
;; after each definition, the value of (stream-ref y 7), what display-stream
;; prints for z, and the final value of sum.
(define (interactions)
  (set! sum 0)
  (let* ((seq (stream-map accum (stream-enumerate-interval 1 20)))
         (sum-after-seq sum)
         (y (stream-filter even? seq))
         (sum-after-y sum)
         (z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
         (sum-after-z sum)
         (y7 (stream-ref y 7))
         (printed (with-output-to-string (lambda () (display-stream z)))))
    (list sum-after-seq sum-after-y sum-after-z y7 printed sum)))

;; seq is the stream of partial sums 1, 3, 6, 10, ..., 210.  Each definition
;; forces only as much of seq as it needs, so sum is 1, 6 and 10 after them.
(check (interactions)
       => '(1 6 10 136 "\n10\n15\n45\n55\n105\n120\n190\n210" 210))

;; Without memoization the elements of seq shared by y and z are recomputed,
;; accum runs more than once for the same x, and all later values differ.
(check (without-memoization interactions)
       => '(1 6 15 162 "\n15\n180\n230\n305" 362))
