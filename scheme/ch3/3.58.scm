(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

;; (expand num den radix) is the stream of digits of num/den, a fraction
;; below 1, written in base radix: each step shifts the remainder one digit
;; to the left and emits the next digit.
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(check (stream-head (expand 1 7 10) 8) => '(1 4 2 8 5 7 1 4))
(check (stream-head (expand 3 8 10) 6) => '(3 7 5 0 0 0))
(check (stream-head (expand 5 8 2) 4) => '(1 0 1 0))
