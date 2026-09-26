(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (mul-streams s1 s2) (stream-map * s1 s2))

;; The nth element (counting from 0) is (n + 1)!.
(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

(check (stream-head (mul-streams integers integers) 4) => '(1 4 9 16))
(check (stream-head factorials 6) => '(1 2 6 24 120 720))
(check (stream-ref factorials 19) => 2432902008176640000)
