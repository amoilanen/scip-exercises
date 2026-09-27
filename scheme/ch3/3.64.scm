(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ((current (stream-car s))
        (next (stream-car (stream-cdr s))))
    (if (< (abs (- next current)) tolerance)
        next
        (stream-limit (stream-cdr s) tolerance))))

(define (stream-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(check (stream-limit (list->stream '(1 5 7 7.5 7.6 7.61)) 0.2) => 7.6)
(check (stream-limit (list->stream '(3 3)) 0.1) => 3)
(check (stream-sqrt 2 1e-3) (=> (approx= 1e-6)) (sqrt 2))
(check (stream-sqrt 144 1e-10) (=> (approx= 1e-12)) 12)
