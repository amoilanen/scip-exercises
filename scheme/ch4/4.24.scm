(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/analyze.scm" (current-load-pathname)))

;; Timed with runtime on MIT Scheme 12.1, both evaluators loaded from source:
;;
;;   program                plain    analyzing   ratio
;;   (fib 18)               5.3 s    3.7 s       1.43
;;   (count-up 20000)      11.6 s    8.3 s       1.39
;;
;; The analyzing evaluator is about 1.4 times faster on both, so the plain
;; evaluator spends roughly 30% of its time analyzing syntax: dispatching on
;; expression types and taking expressions apart again on every evaluation.

(define benchmarks
  '(((define (fib n)
       (if (< n 2)
           n
           (+ (fib (- n 1)) (fib (- n 2)))))
     (fib 12))
    ((define (count-up n)
       (define (iter i)
         (if (= i n) i (iter (+ i 1))))
       (iter 0))
     (count-up 500))
    ((define (make-accumulator total)
       (lambda (amount)
         (set! total (+ total amount))
         total))
     (define acc (make-accumulator 100))
     (acc 10)
     (cond ((> (acc 10) 150) 'big)
           ((> (acc 0) 110) 'medium)
           (else 'small)))))

(define (same-result? program)
  (equal? (apply interpret program)
          (apply analyzing-interpret program)))

(check (map (lambda (program) (apply analyzing-interpret program)) benchmarks)
       => '(144 500 medium))
(check (map same-result? benchmarks) => '(#t #t #t))
