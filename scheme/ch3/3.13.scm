(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; z is a circular list: the cdr of the third pair points back to the first.
;;   z -> [a|*]->[b|*]->[c|*]
;;         ^                |
;;         +----------------+
;; (last-pair z) never finds a pair whose cdr is empty, so it loops forever.

(define z (make-cycle (list 'a 'b 'c)))
(check (eq? (cdddr z) z) => #t)
(check (list-ref z 3) => 'a)
(check (list-ref z 100) => 'b)
