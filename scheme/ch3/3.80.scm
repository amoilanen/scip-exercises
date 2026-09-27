(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "3.77.scm" (current-load-pathname)))

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (- (/ R L)))))
    (stream-map cons vC iL)))

(define (state-close? tolerance)
  (let ((close? (approx= tolerance)))
    (lambda (state expected)
      (and (close? (car state) (car expected))
           (close? (cdr state) (cdr expected))))))

(define (states-close? tolerance)
  (lambda (states expected)
    (and (= (length states) (length expected))
         (every (state-close? tolerance) states expected))))

(define RLC1 (RLC 1 1 0.2 0.1))

(check (stream-head (RLC1 10 0) 4)
       (=> (states-close? 1e-12))
       '((10 . 0) (10 . 1) (9.5 . 1.9) (8.55 . 2.66)))

;; The circuit is underdamped: the oscillation dies out.
(check (stream-ref (RLC1 10 0) 1000)
       (=> (state-close? 1e-6))
       '(0 . 0))
