(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;; append copies the pairs of x, so x is still (a b) and (cdr x) is (b):
;;   x -> [a|*]->[b|/]
;;   z -> [a|*]->[b|*]->[c|*]->[d|/]    (the last two pairs are y)
;;
;; append! changes the cdr of the last pair of x to point to y, so x and w
;; are the same list and (cdr x) is (b c d):
;;   x, w -> [a|*]->[b|*]->[c|*]->[d|/]

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(check z => '(a b c d))
(check (cdr x) => '(b))
(check (eq? (cddr z) y) => #t)

(define w (append! x y))
(check w => '(a b c d))
(check (cdr x) => '(b c d))
(check (eq? w x) => #t)
