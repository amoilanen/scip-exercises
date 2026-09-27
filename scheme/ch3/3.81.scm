(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/streams.scm" (current-load-pathname)))

;; A linear congruential generator.
(define random-modulus (expt 2 32))

(define (rand-update x)
  (modulo (+ (* 1664525 x) 1013904223) random-modulus))

(define random-init 12345)

;; Answers each request in the stream with a number: generate with the next
;; number of the sequence, (reset x) with x itself, restarting the sequence
;; from x.
(define (random-numbers requests)
  (define (respond requests last)
    (if (stream-null? requests)
        the-empty-stream
        (let ((value (next-value (stream-car requests) last)))
          (cons-stream value (respond (stream-cdr requests) value)))))
  (respond requests random-init))

(define (next-value request last)
  (cond ((eq? request 'generate) (rand-update last))
        ((and (pair? request) (eq? (car request) 'reset))
         (cadr request))
        (else (error "Unknown request -- RANDOM-NUMBERS" request))))

(define (responses requests)
  (stream->list (random-numbers (list->stream requests))))

(check (responses '(generate generate))
       => (list (rand-update random-init)
                (rand-update (rand-update random-init))))
(check (responses '((reset 7) generate)) => (list 7 (rand-update 7)))

(check (let ((result (responses '((reset 7) generate generate
                                  (reset 7) generate generate))))
         (equal? (list-head result 3) (list-tail result 3)))
       => #t)
(check (let ((result (responses `(generate generate
                                  (reset ,random-init) generate generate))))
         (equal? (list-head result 2) (list-tail result 3)))
       => #t)
(check-error (responses '(generate shuffle)))
