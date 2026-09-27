(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

;; mystery reverses a list in place: it walks down x, pointing the cdr of each
;; pair back to the previously visited pair.
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; Before: v -> [a|*]->[b|*]->[c|*]->[d|/]
;; After:  w -> [d|*]->[c|*]->[b|*]->[a|/] <- v
;; v still points to the pair holding a, which is now the last pair, so v
;; prints as (a) and w as (d c b a).

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
(check v => '(a))
(check w => '(d c b a))
(check (eq? (cdddr w) v) => #t)
(check (mystery '()) => '())
