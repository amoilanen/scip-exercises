(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (cycle? x)
  (let loop ((x x) (visited '()))
    (cond ((not (pair? x)) #f)
          ((memq x visited) #t)
          (else (loop (cdr x) (cons x visited))))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (attach-tail-cycle! x)
  (set-cdr! (last-pair x) (make-cycle (list 'c 'd)))
  x)

(define shared
  (let* ((x (list 'a))
         (y (cons x x)))
    (cons y y)))

(check (cycle? '()) => #f)
(check (cycle? (list 'a 'b 'c)) => #f)
(check (cycle? shared) => #f)
(check (cycle? (make-cycle (list 'a))) => #t)
(check (cycle? (make-cycle (list 'a 'b 'c))) => #t)
(check (cycle? (attach-tail-cycle! (list 'a 'b))) => #t)
