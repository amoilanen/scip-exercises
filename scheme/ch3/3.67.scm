(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (all-pairs s t)
  (let ((s0 (stream-car s))
        (t0 (stream-car t)))
    (cons-stream
     (list s0 t0)
     (interleave
      (interleave (stream-map (lambda (x) (list s0 x)) (stream-cdr t))
                  (stream-map (lambda (x) (list x t0)) (stream-cdr s)))
      (all-pairs (stream-cdr s) (stream-cdr t))))))

(define (grid n)
  (append-map (lambda (i) (map (lambda (j) (list i j)) (iota n 1)))
              (iota n 1)))

(define prefix (stream-head (all-pairs integers integers) 200))

(define (in-prefix? pair)
  (if (member pair prefix) #t #f))

(check (stream-head (all-pairs integers integers) 6)
       => '((1 1) (1 2) (2 2) (2 1) (2 3) (1 3)))
(check (length (delete-duplicates prefix)) => 200)
(check (every in-prefix? (grid 5)) => #t)
