(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval-compiled.scm" (current-load-pathname)))

(define factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define (statistics->list statistics)
  (list (cdr (assq 'total-pushes statistics))
        (cdr (assq 'maximum-depth statistics))))

(define (check-fit measure pushes depth ns)
  (for-each (lambda (n)
              (check (measure n) => (list (pushes n) (depth n))))
            ns))

(define (interpreted n)
  (eceval-run eceval factorial `(factorial ,n))
  (statistics->list (stack-statistics eceval)))

(define (compiled-with machine)
  (compile-and-go machine factorial)
  (lambda (n)
    (eceval-interpret machine `(factorial ,n))
    (statistics->list (stack-statistics machine))))

(define factorial-machine
  (make-machine
   '(n val continue)
   (list (list '= =) (list '- -) (list '* *))
   '((perform (op initialize-stack))
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done)))

(define (special-purpose n)
  (set-register-contents! factorial-machine 'n n)
  (start factorial-machine)
  (statistics->list (stack-statistics factorial-machine)))

;; a.              total pushes   maximum depth
;;   interpreted     32n - 16        5n + 3
;;   compiled         6n + 1         3n - 1
;;   special          2n - 2         2n - 2
;;
;; For large n the compiled code does 6/32 (about 19%) of the pushes of the
;; interpreter and needs 3/5 of its stack; the special-purpose machine does
;; 2/32 (about 6%) of the pushes and needs 2/5 of the stack.  The compiled
;; figures include the interpreted call (factorial n) that starts it.

(define ns '(2 3 4 5 8 10))

(check-fit interpreted
           (lambda (n) (- (* 32 n) 16))
           (lambda (n) (+ (* 5 n) 3))
           ns)
(check-fit (compiled-with compiled-eceval)
           (lambda (n) (+ (* 6 n) 1))
           (lambda (n) (- (* 3 n) 1))
           ns)
(check-fit special-purpose
           (lambda (n) (- (* 2 n) 2))
           (lambda (n) (- (* 2 n) 2))
           ns)

;; b. Most of the difference comes from calling =, - and * as procedures:
;; the compiler can't know that they leave the registers alone, so it saves
;; registers around these calls as well.  Open-coding them (exercise 5.38)
;; leaves two saves per call, of continue and env around the recursive call,
;; just as the hand-written machine saves continue and n.  What remains is
;; the constant cost of entering from the interpreter and of the test
;; whether factorial is a primitive or a compiled procedure.

(load (merge-pathnames "lib/open-coding.scm" (current-load-pathname)))

(check-fit (compiled-with (make-compiled-eceval eceval-dispatch-table '() '()))
           (lambda (n) (+ (* 2 n) 3))
           (lambda (n) (- (* 2 n) 2))
           '(3 4 5 8 10))
