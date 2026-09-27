(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

(define factorial-machine (make-factorial-machine))

(define (factorial-statistics n)
  (run-machine factorial-machine (list (list 'n n)) 'val)
  (list n
        (cdr (assq 'total-pushes (stack-statistics factorial-machine)))
        (cdr (assq 'maximum-depth (stack-statistics factorial-machine)))))

(check (map factorial-statistics (iota 5 1))
       => '((1 0 0) (2 2 2) (3 4 4) (4 6 6) (5 8 8)))

;; Each of the n - 1 recursive calls pushes continue and n, and nothing is
;; popped before the base case, so both the total number of pushes and the
;; maximum depth are 2(n - 1).
(for-each (lambda (n)
            (check (factorial-statistics n)
                   => (list n (* 2 (- n 1)) (* 2 (- n 1)))))
          (iota 20 1))
