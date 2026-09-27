(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval-compiled.scm" (current-load-pathname)))

;; (compile-and-run exp) evaluates exp, compiles the resulting expression
;; in the current environment and runs the compiled code, which returns
;; through continue like any other evaluation.  The code is assembled into
;; the running machine, so the operation that assembles it must know the
;; machine.

(define (compile-and-run? exp) (tagged-list? exp 'compile-and-run))
(define (compile-and-run-operand exp) (cadr exp))

(define compile-and-run-code
  '(ev-compile-and-run
    (save continue)
    (save env)
    (assign exp (op compile-and-run-operand) (reg exp))
    (assign continue (label ev-compile-and-run-assemble))
    (goto (label eval-dispatch))
    ev-compile-and-run-assemble
    (restore env)
    (restore continue)
    (assign val (op assemble-compiled) (reg val))
    (goto (reg val))))

(define compile-and-run-eceval
  (make-compiled-eceval
   (cons '(compile-and-run? ev-compile-and-run) eceval-dispatch-table)
   compile-and-run-code
   (list (list 'compile-and-run? compile-and-run?)
         (list 'compile-and-run-operand compile-and-run-operand)
         (list 'assemble-compiled
               (lambda (exp)
                 (assemble-compiled exp compile-and-run-eceval))))))

(define (ec-eval exp)
  (eceval-interpret compile-and-run-eceval exp))

(set! the-global-environment (setup-environment))

(check (ec-eval '(compile-and-run
                  '(define (factorial n)
                     (if (= n 1)
                         1
                         (* (factorial (- n 1)) n)))))
       => 'ok)
(check (compiled-procedure? (ec-eval 'factorial)) => #t)
(check (ec-eval '(factorial 5)) => 120)

(check (ec-eval '(compile-and-run '(+ 1 2))) => 3)
(check (ec-eval '(+ 1 (compile-and-run '(factorial 3)))) => 7)
(check (ec-eval '(compile-and-run (list '* 6 7))) => 42)

(check (ec-eval '(define (make-adder n)
                   (compile-and-run '(lambda (x) (+ x n)))))
       => 'ok)
(check (ec-eval '((make-adder 10) 5)) => 15)

;; The compiled factorial behaves as in exercise 5.45: 6n + 1 pushes and a
;; maximum depth of 3n - 1.
(ec-eval '(factorial 10))
(check (stack-statistics compile-and-run-eceval)
       => '((total-pushes . 61) (maximum-depth . 29)))
