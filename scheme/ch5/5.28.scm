(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval.scm" (current-load-pathname)))

;; eceval was built with the tail-recursive sequence code when the library
;; was loaded; this definition only affects machines made from now on.
(define sequence-code
  '(ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))
    ev-sequence
    (test (op no-more-exps?) (reg unev))
    (branch (label ev-sequence-end))
    (assign exp (op first-exp) (reg unev))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-end
    (restore continue)
    (goto (reg continue))))

(define non-tail-eceval (make-eceval eceval-dispatch-table '() '()))

(define iterative-factorial
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product) (+ counter 1))))
     (iter 1 1)))

(define recursive-factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define (factorial-statistics machine definition n)
  (eceval-run machine definition (list 'factorial n))
  (stack-statistics machine))

;; Without tail recursion even the iterative factorial needs space that grows
;; with n:
;;
;;                         Maximum depth   Number of pushes
;;   Recursive factorial      8n + 3          34n - 16
;;   Iterative factorial      3n + 14         37n + 33
;;
;; compared with 5n + 3, 32n - 16 and 10, 35n + 29 in exercises 5.26 and 5.27.

(define (check-statistics machine definition depth pushes)
  (for-each
   (lambda (n)
     (check (factorial-statistics machine definition n)
            => `((total-pushes . ,(pushes n))
                 (maximum-depth . ,(depth n)))))
   '(1 2 3 4 5 10 20)))

(check (eceval-run non-tail-eceval iterative-factorial '(factorial 5)) => 120)
(check (eceval-run non-tail-eceval recursive-factorial '(factorial 5)) => 120)
(check (eceval-run non-tail-eceval '(begin 1 2 3)) => 3)

(check-statistics non-tail-eceval recursive-factorial
                  (lambda (n) (+ (* 8 n) 3))
                  (lambda (n) (- (* 34 n) 16)))
(check-statistics non-tail-eceval iterative-factorial
                  (lambda (n) (+ (* 3 n) 14))
                  (lambda (n) (+ (* 37 n) 33)))

(check-statistics eceval iterative-factorial
                  (lambda (n) 10)
                  (lambda (n) (+ (* 35 n) 29)))
