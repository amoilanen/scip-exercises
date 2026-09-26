(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S
  (cons-stream 1 (merge (scale-stream S 2)
                        (merge (scale-stream S 3) (scale-stream S 5)))))

(check (stream->list (merge (list->stream '(1 3 5 6))
                            (list->stream '(2 3 6 7))))
       => '(1 2 3 5 6 7))
(check (stream-head S 15) => '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24))
(check (stream-ref S 1690) => 2125764000)
