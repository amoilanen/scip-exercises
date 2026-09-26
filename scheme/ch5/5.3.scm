(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

;; Version 1 data paths: registers x and guess; operations good-enough?
;; (reads guess and x, feeds the test) and improve (reads guess and x);
;; buttons guess<-1.0 and guess<-improve.

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define sqrt-controller
  '((assign guess (const 1.0))
    sqrt-iter
      (test (op good-enough?) (reg guess) (reg x))
      (branch (label sqrt-done))
      (assign guess (op improve) (reg guess) (reg x))
      (goto (label sqrt-iter))
    sqrt-done))

(define (sqrt-1 x)
  (run-machine (make-machine '(x guess)
                             (list (list 'good-enough? good-enough?)
                                   (list 'improve improve))
                             sqrt-controller)
               (list (list 'x x))
               'guess))

;; Version 2 data paths: registers x, guess and a temporary t; operations
;; *, -, / and + on those registers and constants, the tests < and >, and
;; negation of t; t has a button for each arithmetic step, guess gets
;; guess<-1.0 and guess<-t/2.

(define sqrt-arithmetic-controller
  '((assign guess (const 1.0))
    sqrt-iter
      (assign t (op *) (reg guess) (reg guess))
      (assign t (op -) (reg t) (reg x))
      (test (op >) (reg t) (const 0))
      (branch (label check-tolerance))
      (assign t (op -) (reg t))
    check-tolerance
      (test (op <) (reg t) (const 0.001))
      (branch (label sqrt-done))
      (assign t (op /) (reg x) (reg guess))
      (assign t (op +) (reg guess) (reg t))
      (assign guess (op /) (reg t) (const 2))
      (goto (label sqrt-iter))
    sqrt-done))

(define (sqrt-2 x)
  (run-machine (make-machine '(x guess t)
                             arithmetic-operations
                             sqrt-arithmetic-controller)
               (list (list 'x x))
               'guess))

(for-each (lambda (x)
            (check (sqrt-1 x) (=> good-enough?) x)
            (check (sqrt-2 x) (=> good-enough?) x))
          '(1 2 9 0.25 1000))

(check (sqrt-1 2) => (sqrt-2 2))
