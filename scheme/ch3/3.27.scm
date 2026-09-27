(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (make-table) (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (and record (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (or (lookup x table)
          (let ((result (f x)))
            (insert! x result table)
            result)))))

;; (define memo-fib (memoize (lambda (n) ...))) creates a frame E1 below the
;; global environment binding f to the lambda, and a frame E2 below E1
;; binding table.  memo-fib is the inner (lambda (x) ...) with environment
;; E2, so every call, including the recursive calls made by f through the
;; global name memo-fib, shares the one table in E2.
;;
;; (memo-fib 3) makes a frame x = 3 below E2.  3 is not in the table, so f
;; is applied in a frame n = 3 below the global environment, where f was
;; created.  That calls memo-fib on 2 and 1 (each a new frame below E2), and
;; f applied to 2 calls memo-fib on 1 and 0.  Whichever request for fib 1
;; comes second finds the value in the table instead of calling f.
;;
;; Each fib(k) for k = 0, ..., n is computed only once, and every other call
;; is a lookup of a recently inserted, hence near the front, record, so
;; memo-fib takes a number of steps proportional to n.
;;
;; (memoize fib) would not work: fib calls itself recursively through the
;; name fib, bypassing the table, so only the outermost result gets stored
;; and the first call is still exponential.

(define computations 0)

(define memo-fib
  (memoize
   (lambda (n)
     (set! computations (+ computations 1))
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))

(check (memo-fib 30) => 832040)
(check computations => 31)
(check (memo-fib 30) => 832040)
(check computations => 31)
(check (memo-fib 32) => 2178309)
(check computations => 33)

(define fib-calls 0)

(define (fib n)
  (set! fib-calls (+ fib-calls 1))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memoized-fib (memoize fib))

(check (memoized-fib 20) => 6765)
(check fib-calls => 21891)
(check (memoized-fib 20) => 6765)
(check fib-calls => 21891)
(check (memoized-fib 19) => 4181)
(check fib-calls => 35420)
