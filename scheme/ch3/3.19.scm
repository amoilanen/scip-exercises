(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

;; Floyd's algorithm: the fast pointer moves two pairs per step and the slow
;; one a single pair.  If there is a cycle, the fast pointer enters it and
;; closes the gap by one pair per step, so they meet; otherwise the fast
;; pointer reaches the end of the list.
(define (cycle? x)
  (define (next x)
    (if (pair? x) (cdr x) '()))
  (let loop ((slow (next x))
             (fast (next (next x))))
    (cond ((not (pair? fast)) #f)
          ((eq? slow fast) #t)
          (else (loop (next slow) (next (next fast)))))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (attach-tail-cycle! x)
  (set-cdr! (last-pair x) (make-cycle (list 'c 'd 'e)))
  x)

(define shared
  (let* ((x (list 'a))
         (y (cons x x)))
    (cons y y)))

(check (cycle? '()) => #f)
(check (cycle? 'a) => #f)
(check (cycle? (list 'a)) => #f)
(check (cycle? (list 'a 'b)) => #f)
(check (cycle? (list 'a 'b 'c 'd 'e)) => #f)
(check (cycle? shared) => #f)
(check (cycle? (make-cycle (list 'a))) => #t)
(check (cycle? (make-cycle (list 'a 'b))) => #t)
(check (cycle? (make-cycle (list 'a 'b 'c 'd 'e))) => #t)
(check (cycle? (attach-tail-cycle! (list 'a 'b))) => #t)
