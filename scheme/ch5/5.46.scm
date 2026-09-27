(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval-compiled.scm" (current-load-pathname)))

(define fib
  '(define (fib n)
     (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2))))))

(define (fibonacci n)
  (let loop ((a 0) (b 1) (n n))
    (if (= n 0) a (loop b (+ a b) (- n 1)))))

(define (statistics->list statistics)
  (list (cdr (assq 'total-pushes statistics))
        (cdr (assq 'maximum-depth statistics))))

(define (check-fit measure pushes depth ns)
  (for-each (lambda (n)
              (check (measure n) => (list (pushes n) (depth n))))
            ns))

(define (interpreted n)
  (eceval-run eceval fib `(fib ,n))
  (statistics->list (stack-statistics eceval)))

(define (compiled-with machine)
  (compile-and-go machine fib)
  (lambda (n)
    (eceval-interpret machine `(fib ,n))
    (statistics->list (stack-statistics machine))))

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '((perform (op initialize-stack))
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label after-fib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
     after-fib-n-1
     (restore n)
     (restore continue)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label after-fib-n-2))
     (save val)
     (goto (label fib-loop))
     after-fib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

(define (special-purpose n)
  (set-register-contents! fib-machine 'n n)
  (start fib-machine)
  (statistics->list (stack-statistics fib-machine)))

;;                 total pushes          maximum depth
;;   interpreted   56 Fib(n + 1) - 40       5n + 3
;;   compiled      10 Fib(n + 1) - 3        3n - 1
;;   open-coded     7 Fib(n + 1)            2n
;;   special        4 Fib(n + 1) - 4        2n - 2
;;
;; The number of pushes grows exponentially for all of them, so their
;; ratios tend to the ratios of the coefficients: the compiled code does
;; 10/56 (about 18%) of the pushes of the interpreter, the special-purpose
;; machine 4/56 (about 7%).  The maximum depths grow linearly with ratios
;; 3/5 and 2/5, as for factorial.

(define ns '(2 3 4 5 8))

(check-fit interpreted
           (lambda (n) (- (* 56 (fibonacci (+ n 1))) 40))
           (lambda (n) (+ (* 5 n) 3))
           ns)
(check-fit (compiled-with compiled-eceval)
           (lambda (n) (- (* 10 (fibonacci (+ n 1))) 3))
           (lambda (n) (- (* 3 n) 1))
           ns)
(check-fit special-purpose
           (lambda (n) (- (* 4 (fibonacci (+ n 1))) 4))
           (lambda (n) (- (* 2 n) 2))
           ns)

(load (merge-pathnames "lib/open-coding.scm" (current-load-pathname)))

(check-fit (compiled-with (make-compiled-eceval eceval-dispatch-table '() '()))
           (lambda (n) (* 7 (fibonacci (+ n 1))))
           (lambda (n) (* 2 n))
           ns)
