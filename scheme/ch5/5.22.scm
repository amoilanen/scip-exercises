(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

(define list-operations
  (list (list 'car car) (list 'cdr cdr) (list 'cons cons)
        (list 'set-cdr! set-cdr!) (list 'null? null?)))

;;; append: a new copy of x in front of y.

(define append-controller
  '((assign continue (label append-done))
    append-loop
      (test (op null?) (reg x))
      (branch (label empty-x))
      (save continue)
      (assign head (op car) (reg x))
      (save head)
      (assign x (op cdr) (reg x))
      (assign continue (label after-append))
      (goto (label append-loop))
    empty-x
      (assign val (reg y))
      (goto (reg continue))
    after-append
      (restore head)
      (restore continue)
      (assign val (op cons) (reg head) (reg val))
      (goto (reg continue))
    append-done))

(define (append-machine x y)
  (run-machine (make-machine '(x y head val continue)
                             list-operations
                             append-controller)
               (list (list 'x x) (list 'y y))
               'val))

;;; append!: splices y onto the last pair of x, which must not be empty.

(define append!-controller
  '((assign last (reg x))
    find-last-pair
      (assign rest (op cdr) (reg last))
      (test (op null?) (reg rest))
      (branch (label splice))
      (assign last (reg rest))
      (goto (label find-last-pair))
    splice
      (perform (op set-cdr!) (reg last) (reg y))))

(define (append!-machine x y)
  (run-machine (make-machine '(x y last rest)
                             list-operations
                             append!-controller)
               (list (list 'x x) (list 'y y))
               'x))

(let* ((x (list 1 2))
       (y (list 3 4))
       (z (append-machine x y)))
  (check z => '(1 2 3 4))
  (check x => '(1 2))
  (check (eq? (cddr z) y) => #t))

(check (append-machine '() '(a)) => '(a))
(check (append-machine '(a) '()) => '(a))
(check (append-machine '() '()) => '())

(let* ((x (list 1 2))
       (y (list 3 4))
       (z (append!-machine x y)))
  (check z => '(1 2 3 4))
  (check (eq? z x) => #t)
  (check (eq? (cddr x) y) => #t))

(let ((x (list 'a)))
  (check (append!-machine x '()) => '(a))
  (check (append!-machine x (list 'b)) => '(a b)))
