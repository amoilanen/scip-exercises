(load "lib/check.scm")
(load "ch4/lib/query.scm")

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) (not (stream-null? s)))
                             stream)))

;; b. The behaviour does not change.  negate, lisp-value and find-assertions
;; map every frame to an empty or a singleton stream.  Interleaving a
;; singleton stream with the rest yields its element followed by the rest,
;; so both flattenings give the same elements in the same order.

(initialize-data-base! microshaft-data-base)

(define queries
  '((job ?x (computer . ?type))
    (and (salary ?person ?amount) (lisp-value > ?amount 30000))
    (and (supervisor ?x ?boss) (not (job ?x (computer programmer))))
    (lives-near ?x ?y)))

(define answers-with-interleaving (map run-query queries))

(define (negate operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (lisp-value call frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (execute (instantiate call
                               frame
                               (lambda (var frame)
                                 (error "Unknown pat var -- LISP-VALUE"
                                        var))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (find-assertions pattern frame)
  (simple-stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)

(check (simple-flatten (stream (stream) (stream 1) (stream) (stream 2)))
       (=> (lambda (s xs) (equal? (stream->list s) xs)))
       '(1 2))
(check (map run-query queries) => answers-with-interleaving)
(check (map length answers-with-interleaving) => '(5 5 6 8))
