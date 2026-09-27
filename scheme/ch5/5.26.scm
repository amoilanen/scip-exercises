(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval.scm" (current-load-pathname)))

(define iterative-factorial
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product) (+ counter 1))))
     (iter 1 1)))

(define (factorial-statistics n)
  (eceval-run eceval iterative-factorial (list 'factorial n))
  (stack-statistics eceval))

;;   n   total pushes   maximum depth
;;   1        64             10
;;   2        99             10
;;   3       134             10
;;   4       169             10
;;   5       204             10
;;
;; a. The maximum depth is 10, independent of n.
;; b. Total pushes = 35n + 29.

(check (eceval-run eceval iterative-factorial '(factorial 5)) => 120)

(for-each
 (lambda (n)
   (check (factorial-statistics n)
          => `((total-pushes . ,(+ (* 35 n) 29))
               (maximum-depth . 10))))
 '(1 2 3 4 5 10 20))
