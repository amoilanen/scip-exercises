(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

(define (sign-change-detector input last-value)
  (cond ((and (< last-value 0) (>= input 0)) 1)
        ((and (>= last-value 0) (< input 0)) -1)
        (else 0)))

(define (make-zero-crossings sense-data)
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(define sense-data
  (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(check (map sign-change-detector '(1 -1 0 -2 3) '(-1 0 -1 -1 2))
       => '(1 -1 1 0 0))
(check (stream->list (make-zero-crossings sense-data))
       => '(0 0 0 0 0 -1 0 0 0 0 1 0 0))
(check (stream->list (make-zero-crossings (list->stream '(-1 -2))))
       => '(-1 0))
