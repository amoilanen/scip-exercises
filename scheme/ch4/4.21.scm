(load "lib/check.scm")
(load "ch4/lib/mceval.scm")

;;; a.

(define factorial-of-10
  '((lambda (n)
      ((lambda (fact) (fact fact n))
       (lambda (ft k)
         (if (= k 1)
             1
             (* k (ft ft (- k 1)))))))
    10))

(define (fibonacci-of n)
  (list '(lambda (n)
           ((lambda (fib) (fib fib n))
            (lambda (fb k)
              (if (< k 2)
                  k
                  (+ (fb fb (- k 1)) (fb fb (- k 2)))))))
        n))

(check (interpret factorial-of-10) => 3628800)
(check (map (lambda (n) (interpret (fibonacci-of n))) '(0 1 2 10))
       => '(0 1 1 55))

;;; b.

(define parity-without-define
  '(define (f x)
     ((lambda (even? odd?)
        (even? even? odd? x))
      (lambda (ev? od? n)
        (if (= n 0) true (od? ev? od? (- n 1))))
      (lambda (ev? od? n)
        (if (= n 0) false (ev? ev? od? (- n 1)))))))

(check (map (lambda (n) (interpret parity-without-define (list 'f n)))
            '(0 1 6 7))
       => '(#t #f #t #f))
