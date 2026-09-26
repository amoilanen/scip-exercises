(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map proc (map stream-cdr argstreams)))))

(check (stream->list (stream-map - (stream-enumerate-interval 1 3)))
       => '(-1 -2 -3))
(check (stream->list (stream-map +
                                 (stream-enumerate-interval 1 3)
                                 (stream-enumerate-interval 10 12)
                                 (stream-enumerate-interval 100 102)))
       => '(111 114 117))
(check (stream-head (stream-map * integers integers) 5) => '(1 4 9 16 25))
(check (stream-null? (stream-map + the-empty-stream the-empty-stream)) => #t)
