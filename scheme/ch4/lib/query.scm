;; The query system of section 4.4.4, run without a driver loop.
;;
;;   (initialize-data-base! items)   empties the data base and adds ITEMS,
;;                                   assertions and (rule ...) forms alike
;;   (add-to-data-base! items)       adds ITEMS to the current data base
;;   (run-query query)               list of the instantiated answers
;;   (run-query-head query n)        at most the first N answers
;;   (run-query-bounded query steps) the answers found before qeval has been
;;                                   called STEPS times, followed by the
;;                                   symbol diverged if the limit was hit
;;
;; microshaft-data-base (4.4.1) and genesis-data-base (exercise 4.63) are
;; ready to be installed.  same-elements? compares answer lists as multisets.

;;; Streams

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (interleave-delayed (force delayed-s2)
                                       (delay (stream-cdr s1))))))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed (stream-car stream)
                          (delay (flatten-stream (stream-cdr stream))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

;;; Operation table used to dispatch on special forms

(define operation-table (make-equal-hash-table))

(define (put op type item)
  (hash-table-set! operation-table (cons op type) item))

(define (get op type)
  (hash-table-ref/default operation-table (cons op type) #f))

;;; Driver

(define (query-frames query)
  (qeval query (singleton-stream empty-frame)))

(define (query-results query)
  (let ((processed (query-syntax-process query)))
    (stream-map (lambda (frame)
                  (instantiate processed
                               frame
                               (lambda (var frame)
                                 (contract-question-mark var))))
                (query-frames processed))))

(define (run-query query)
  (stream->list (query-results query)))

(define (run-query-head query n)
  (let loop ((results (query-results query)) (n n))
    (if (or (= n 0) (stream-null? results))
        '()
        (cons (stream-car results)
              (loop (stream-cdr results) (- n 1))))))

(define (run-query-bounded query step-limit)
  (let ((found '())
        (plain-qeval qeval))
    (call-with-current-continuation
     (lambda (give-up)
       (define (counting-qeval query frame-stream)
         (set! step-limit (- step-limit 1))
         (if (< step-limit 0)
             (give-up 'diverged))
         (plain-qeval query frame-stream))
       (dynamic-wind
        (lambda () (set! qeval counting-qeval))
        (lambda ()
          (stream-for-each (lambda (answer) (set! found (cons answer found)))
                           (query-results query)))
        (lambda () (set! qeval plain-qeval)))
       (reverse found)))
    (if (< step-limit 0)
        (reverse (cons 'diverged found))
        (reverse found))))

(define (same-elements? xs ys)
  (define (remove-one x ys)
    (cond ((null? ys) #f)
          ((equal? x (car ys)) (cdr ys))
          (else (let ((rest (remove-one x (cdr ys))))
                  (and rest (cons (car ys) rest))))))
  (cond ((null? xs) (null? ys))
        ((remove-one (car xs) ys)
         => (lambda (rest) (same-elements? (cdr xs) rest)))
        (else #f)))
