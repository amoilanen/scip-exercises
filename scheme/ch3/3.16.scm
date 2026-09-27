(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Each structure below is made of exactly three pairs.

;; [*|*]->[*|*]->[*|/]
;;  a      b      c
(define three (list 'a 'b 'c))

;; [*|*]->[*|/]
;;  |      |
;;  +------+->[a|/]
(define four
  (let ((x (list 'a)))
    (list x x)))

;; [*|*]
;;  | |
;; [*|*]
;;  | |
;; [a|/]
(define seven
  (let* ((x (list 'a))
         (y (cons x x)))
    (cons y y)))

;; The cdr of the last pair points back to the first, so count-pairs never
;; returns.
(define (make-cycle)
  (let ((x (list 'a 'b 'c)))
    (set-cdr! (cddr x) x)
    x))

(check (count-pairs three) => 3)
(check (count-pairs four) => 4)
(check (count-pairs seven) => 7)
(check (let ((cycle (make-cycle))) (eq? (cdddr cycle) cycle)) => #t)
