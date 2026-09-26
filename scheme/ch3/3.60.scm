(load "lib/check.scm")
(load "ch3/3.59.scm")

;; (a0 + A)(b0 + B) = a0 b0 + (a0 B + A (b0 + B)), where A and B are the
;; series without their constant terms.
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(check (stream-head (mul-series ones ones) 5) => '(1 2 3 4 5))
(check (stream-head (add-streams (mul-series sine-series sine-series)
                                 (mul-series cosine-series cosine-series))
                    8)
       => '(1 0 0 0 0 0 0 0))
