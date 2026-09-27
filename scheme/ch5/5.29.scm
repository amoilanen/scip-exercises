(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval.scm" (current-load-pathname)))

(define tree-fib
  '(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-statistics n)
  (eceval-run eceval tree-fib (list 'fib n))
  (stack-statistics eceval))

(define (maximum-depth n) (cdr (assq 'maximum-depth (fib-statistics n))))
(define (total-pushes n) (cdr (assq 'total-pushes (fib-statistics n))))

(define (fib n)
  (let iter ((a 0) (b 1) (count n))
    (if (= count 0)
        a
        (iter b (+ a b) (- count 1)))))

;;   n   total pushes   maximum depth
;;   2        72             13
;;   3       128             18
;;   4       240             23
;;   5       408             28
;;   6       688             33
;;
;; a. The maximum depth is 5n + 3 for n >= 1.
;; b. S(n) = S(n-1) + S(n-2) + 40, with S(0) = S(1) = 16, which gives
;;    S(n) = 56 Fib(n+1) - 40, i.e. a = 56 and b = -40.

(check (eceval-run eceval tree-fib '(fib 10)) => 55)

(for-each
 (lambda (n)
   (check (maximum-depth n) => (+ (* 5 n) 3)))
 '(1 2 3 4 5 6 10))

(for-each
 (lambda (n)
   (check (total-pushes n) => (- (* 56 (fib (+ n 1))) 40)))
 '(0 1 2 3 4 5 6 10))

(for-each
 (lambda (n)
   (check (total-pushes n)
          => (+ (total-pushes (- n 1)) (total-pushes (- n 2)) 40)))
 '(2 3 4 5 6))
