(load "lib/check.scm")
(load "ch3/3.70.scm")

(define sum-of-cubes (pair-weight (lambda (i j) (+ (cube i) (cube j)))))

;; Pairs of equal weight are adjacent in a stream ordered by weight.
(define (equal-weight-neighbours s weight)
  (let ((w1 (weight (stream-car s)))
        (w2 (weight (stream-car (stream-cdr s)))))
    (if (= w1 w2)
        (cons-stream w1
                     (equal-weight-neighbours (stream-cdr (stream-cdr s))
                                              weight))
        (equal-weight-neighbours (stream-cdr s) weight))))

(define ramanujan-numbers
  (equal-weight-neighbours (weighted-pairs integers integers sum-of-cubes)
                           sum-of-cubes))

(check (stream-head ramanujan-numbers 6)
       => '(1729 4104 13832 20683 32832 39312))
