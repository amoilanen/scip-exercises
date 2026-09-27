(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "5.4.scm" (current-load-pathname)))

(define (expt-machine-run make-expt-machine result-register b n)
  (let* ((machine (make-expt-machine))
         (value (run-machine machine
                             (list (list 'b b) (list 'n n))
                             result-register)))
    (cons value (stack-statistics machine))))

(define (recursive-run b n)
  (expt-machine-run make-expt-recursive-machine 'val b n))

(define (iterative-run b n)
  (expt-machine-run make-expt-iterative-machine 'product b n))

(for-each (lambda (b)
            (for-each (lambda (n)
                        (check (car (recursive-run b n)) => (expt b n))
                        (check (car (iterative-run b n)) => (expt b n)))
                      '(0 1 2 3 7 20)))
          '(0 1 2 -3 1/2))

;; The recursive machine saves continue once per multiplication; the
;; iterative one never touches the stack.
(check (recursive-run 3 5) => '(243 (total-pushes . 5) (maximum-depth . 5)))
(check (iterative-run 3 5) => '(243 (total-pushes . 0) (maximum-depth . 0)))
