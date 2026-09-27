(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (case arg
        ((how-many-calls?) calls)
        ((reset-count) (set! calls 0) calls)
        (else (set! calls (+ calls 1))
              (f arg))))))

(define s (make-monitored sqrt))
(check (s 'how-many-calls?) => 0)
(check (s 100) => 10)
(check (s 25) => 5)
(check (s 'how-many-calls?) => 2)
(s 'reset-count)
(check (s 'how-many-calls?) => 0)
