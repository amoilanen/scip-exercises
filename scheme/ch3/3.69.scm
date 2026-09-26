(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (pair) (cons (stream-car s) pair))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pythagorean? triple)
  (let ((i (car triple))
        (j (cadr triple))
        (k (caddr triple)))
    (= (+ (square i) (square j)) (square k))))

(define pythagorean-triples
  (stream-filter pythagorean? (triples integers integers integers)))

(define (ordered? triple)
  (apply <= triple))

(check (stream-head (triples integers integers integers) 4)
       => '((1 1 1) (1 1 2) (2 2 2) (1 2 2)))
(check (every ordered? (stream-head (triples integers integers integers) 500))
       => #t)
(check (stream-head pythagorean-triples 3) => '((3 4 5) (6 8 10) (5 12 13)))
