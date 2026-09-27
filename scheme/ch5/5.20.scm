(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

;; (define x (cons 1 2))
;; (define y (list x x))
;;
;; Box and pointer: y is a list of two pairs whose cars both point to x.
;;
;;   y --> [ * | * ]---> [ * | / ]
;;           |             |
;;           +------+------+
;;                  v
;;           x --> [ 1 | 2 ]
;;
;; Memory vectors, with the free pointer initially p1. x is allocated first;
;; (list x x) allocates its last pair before the first one:
;;
;;   index      0    1    2    3    4
;;   the-cars        n1   p1   p1
;;   the-cdrs        n2   e0   p2
;;
;; x is p1, y is p3, and free ends up as p4.
;;
;; The allocation below reproduces the table, writing typed values as the
;; symbols p1, n1, e0 and so on.

(define memory-size 5)
(define the-cars (make-vector memory-size '-))
(define the-cdrs (make-vector memory-size '-))
(define free 1)

(define (typed-value type value) (symbol type value))

(define (encode datum)
  (cond ((number? datum) (typed-value 'n datum))
        ((null? datum) (typed-value 'e 0))
        ((symbol? datum) datum)
        (else (error "Cannot store in memory:" datum))))

(define (memory-cons a d)
  (if (>= free memory-size)
      (error "Out of memory"))
  (let ((index free))
    (vector-set! the-cars index (encode a))
    (vector-set! the-cdrs index (encode d))
    (set! free (+ free 1))
    (typed-value 'p index)))

(define (memory-list . items)
  (fold-right memory-cons '() items))

(define x (memory-cons 1 2))
(define y (memory-list x x))

(check x => 'p1)
(check y => 'p3)
(check free => 4)
(check the-cars => #(- n1 p1 p1 -))
(check the-cdrs => #(- n2 e0 p2 -))

(memory-cons 3 4)
(check-error (memory-cons 5 6))
