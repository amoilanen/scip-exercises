(load "lib/check.scm")
(load "ch5/lib/eceval.scm")

(define recursive-factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define (factorial-statistics n)
  (eceval-run eceval recursive-factorial (list 'factorial n))
  (stack-statistics eceval))

;;   n   total pushes   maximum depth
;;   1        16              8
;;   2        48             13
;;   3        80             18
;;   4       112             23
;;   5       144             28
;;
;;                         Maximum depth   Number of pushes
;;   Recursive factorial      5n + 3          32n - 16
;;   Iterative factorial        10            35n + 29

(check (eceval-run eceval recursive-factorial '(factorial 5)) => 120)

(for-each
 (lambda (n)
   (check (factorial-statistics n)
          => `((total-pushes . ,(- (* 32 n) 16))
               (maximum-depth . ,(+ (* 5 n) 3)))))
 '(1 2 3 4 5 10 20))
