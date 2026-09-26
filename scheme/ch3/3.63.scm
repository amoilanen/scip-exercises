(load "lib/check.scm")
(load "ch3/lib/optional-memoization.scm")

(define improvements 0)

(define (sqrt-improve guess x)
  (set! improvements (+ improvements 1))
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (louis-sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess) (sqrt-improve guess x))
                           (louis-sqrt-stream x))))

(define (improvements-for-guess make-stream n)
  (set! improvements 0)
  (stream-ref (make-stream 2) n)
  improvements)

(check (stream-head (louis-sqrt-stream 2) 5)
       => (stream-head (sqrt-stream 2) 5))

;; sqrt-stream maps over guesses, the very stream it defines, so the
;; memoized guess n - 1 is improved once to get guess n.  Louis's version
;; maps over a new call (louis-sqrt-stream x), a stream that shares nothing
;; with the one being built, so every guess is computed from scratch:
;; n(n + 1)/2 improvements for the nth guess instead of n.
(check (map (lambda (n) (improvements-for-guess sqrt-stream n)) '(1 5 10))
       => '(1 5 10))
(check (map (lambda (n) (improvements-for-guess louis-sqrt-stream n))
            '(1 5 10))
       => '(1 15 55))

;; Without memoization the two versions would be equally inefficient:
;; forcing the tail of guesses would recompute every earlier guess as well.
(check (without-memoization
        (lambda ()
          (map (lambda (n)
                 (list (improvements-for-guess sqrt-stream n)
                       (improvements-for-guess louis-sqrt-stream n)))
               '(1 5 10))))
       => '((1 1) (15 15) (55 55)))
