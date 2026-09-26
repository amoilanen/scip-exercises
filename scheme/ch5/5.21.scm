(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

(define list-operations
  (list (list 'car car) (list 'cdr cdr)
        (list 'null? null?) (list 'pair? pair?) (list '+ +)))

;;; a. Recursive count-leaves.

(define count-leaves-controller
  '((assign continue (label count-done))
    count-loop
      (test (op null?) (reg tree))
      (branch (label empty-tree))
      (test (op pair?) (reg tree))
      (branch (label count-car))
      (assign val (const 1))
      (goto (reg continue))
    empty-tree
      (assign val (const 0))
      (goto (reg continue))
    count-car
      (save continue)
      (save tree)
      (assign continue (label after-car))
      (assign tree (op car) (reg tree))
      (goto (label count-loop))
    after-car
      (restore tree)
      (save val)
      (assign continue (label after-cdr))
      (assign tree (op cdr) (reg tree))
      (goto (label count-loop))
    after-cdr
      (assign cdr-leaves (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg cdr-leaves))
      (goto (reg continue))
    count-done))

(define (count-leaves-recursive tree)
  (run-machine (make-machine '(tree val cdr-leaves continue)
                             list-operations
                             count-leaves-controller)
               (list (list 'tree tree))
               'val))

;;; b. Count-leaves with an explicit counter. Counting the cdr is the last
;;; step, so it needs no new continuation.

(define count-leaves-iterative-controller
  '((assign n (const 0))
    (assign continue (label count-done))
    count-loop
      (test (op null?) (reg tree))
      (branch (label empty-tree))
      (test (op pair?) (reg tree))
      (branch (label count-car))
      (assign n (op +) (reg n) (const 1))
      (goto (reg continue))
    empty-tree
      (goto (reg continue))
    count-car
      (save continue)
      (save tree)
      (assign continue (label after-car))
      (assign tree (op car) (reg tree))
      (goto (label count-loop))
    after-car
      (restore tree)
      (restore continue)
      (assign tree (op cdr) (reg tree))
      (goto (label count-loop))
    count-done))

(define (count-leaves-iterative tree)
  (run-machine (make-machine '(tree n continue)
                             list-operations
                             count-leaves-iterative-controller)
               (list (list 'tree tree))
               'n))

(define trees
  '(() a (a) (a b c) (() ()) ((a b) (c (d e)) f) (((a . b) . c) . d)))

(check (map count-leaves-recursive trees) => '(0 1 1 3 0 6 4))
(check (map count-leaves-iterative trees) => '(0 1 1 3 0 6 4))
