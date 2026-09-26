(load "lib/check.scm")
(load "ch3/lib/streams.scm")

;; interleave takes every other element from the pairs of the first row, so
;; the pairs of row i appear every 2^i steps, starting after the diagonal
;; element (i i) and then half a period later with (i i+1).  The number of
;; pairs preceding (i j) is therefore
;;
;;   2^i - 2                   when j = i,
;;   2^(i-1) (2(j - i) + 1) - 2   when j > i.
(define (pairs-preceding i j)
  (if (= i j)
      (- (expt 2 i) 2)
      (- (* (expt 2 (- i 1)) (+ (* 2 (- j i)) 1)) 2)))

(define (all-positions-match? s count)
  (let loop ((s s) (position 0))
    (or (= position count)
        (let ((pair (stream-car s)))
          (and (= (pairs-preceding (car pair) (cadr pair)) position)
               (loop (stream-cdr s) (+ position 1)))))))

(check (stream-head (pairs integers integers) 7)
       => '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3)))
(check (all-positions-match? (pairs integers integers) 2000) => #t)

(check (pairs-preceding 1 100) => 197)
(check (pairs-preceding 99 100) => (- (* 3 (expt 2 98)) 2))
(check (pairs-preceding 100 100) => (- (expt 2 100) 2))
