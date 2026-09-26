(load "lib/check.scm")
(load "ch3/lib/optional-memoization.scm")

(define additions 0)

(define (counted-add a b)
  (set! additions (+ additions 1))
  (+ a b))

(define (make-fibs)
  (define fibs
    (cons-stream 0
                 (cons-stream 1
                              (stream-map counted-add (stream-cdr fibs) fibs))))
  fibs)

(define (additions-for-fib n)
  (set! additions 0)
  (stream-ref (make-fibs) n)
  additions)

(define (fib n)
  (let iter ((a 0) (b 1) (count n))
    (if (= count 0)
        a
        (iter b (+ a b) (- count 1)))))

(check (stream-head (make-fibs) 10) => '(0 1 1 2 3 5 8 13 21 34))

;; With memoization each element is computed once, from the two before it,
;; so the nth Fibonacci number takes n - 1 additions.
(check (map additions-for-fib '(1 2 3 10 20)) => '(0 1 2 9 19))

;; Without it, computing element k forces fresh copies of elements k - 1 and
;; k - 2, so it takes C(k) = C(k - 1) + C(k - 2) + 1 = Fib(k + 1) - 1
;; additions.  stream-ref pays that for every element on its way to the nth,
;; Fib(n + 3) - n - 2 additions in total: exponential in n.
(check (without-memoization
        (lambda () (map additions-for-fib '(1 2 3 10 20))))
       => (map (lambda (n) (- (fib (+ n 3)) n 2)) '(1 2 3 10 20)))
