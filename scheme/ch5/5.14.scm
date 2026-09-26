(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

(define factorial-machine (make-factorial-machine))

(define (factorial-statistics n)
  (run-machine factorial-machine (list (list 'n n)) 'val)
  (list n
        (cdr (assq 'total-pushes (stack-statistics factorial-machine)))
        (cdr (assq 'maximum-depth (stack-statistics factorial-machine)))))

(define (iota-from-1 count) (iota count 1))

(check (map factorial-statistics (iota-from-1 5))
       => '((1 0 0) (2 2 2) (3 4 4) (4 6 6) (5 8 8)))

;; Each of the n - 1 recursive calls pushes continue and n, and nothing is
;; popped before the base case, so both the total number of pushes and the
;; maximum depth are 2(n - 1).
(for-each (lambda (n)
            (check (factorial-statistics n)
                   => (list n (* 2 (- n 1)) (* 2 (- n 1)))))
          (iota-from-1 20))
