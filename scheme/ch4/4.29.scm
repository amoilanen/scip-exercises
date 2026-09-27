(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/lazy.scm" (current-load-pathname)))

(define memoizing-force-it force-it)

(define (non-memoizing-force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; A procedure that uses its argument many times recomputes the argument on
;; every use without memoization: here (fib 10) is computed ten times
;; instead of once, so fib is called ten times as often.

(define fib-program
  '((define fib-calls 0)
    (define (fib n)
      (set! fib-calls (+ fib-calls 1))
      (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
    (define (add-n-times x n)
      (if (= n 0)
          0
          (+ x (add-n-times x (- n 1)))))
    (define sum (add-n-times (fib 10) 10))))

(define (fib-program-result)
  (apply interpret (append fib-program '((list sum fib-calls)))))

;; square uses x twice, so (id 10) runs twice unless the thunk is memoized:
;;
;;                     memoized   not memoized
;;   (square (id 10))    100          100
;;   count                 1            2

(define square-program
  '((define count 0)
    (define (id x)
      (set! count (+ count 1))
      x)
    (define (square x) (* x x))
    (define result (square (id 10)))))

(define (square-program-result)
  (apply interpret (append square-program '((list result count)))))

(define force-it memoizing-force-it)
(check (fib-program-result) => '(550 177))
(check (square-program-result) => '(100 1))

(define force-it non-memoizing-force-it)
(check (fib-program-result) => '(550 1770))
(check (square-program-result) => '(100 2))
