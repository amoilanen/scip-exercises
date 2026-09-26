(load "lib/check.scm")
(load "ch5/lib/eceval.scm")

;; a. When the operator is a symbol, it is looked up directly, so env and
;; unev need not be saved around its evaluation.

(define application-code
  (append
   '(ev-application
     (save continue)
     (assign unev (op operands) (reg exp))
     (assign exp (op operator) (reg exp))
     (test (op variable?) (reg exp))
     (branch (label ev-appl-symbol-operator))
     (save env)
     (save unev)
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     ev-appl-did-operator
     (restore unev)
     (restore env)
     (assign proc (reg val))
     (goto (label ev-appl-operands))
     ev-appl-symbol-operator
     (assign proc (op lookup-variable-value) (reg exp) (reg env))
     ev-appl-operands
     (assign argl (op empty-arglist))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc))
   (member 'ev-appl-operand-loop application-code)))

(define symbol-operator-eceval
  (make-eceval eceval-dispatch-table '() '()))

(define factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define (total-pushes machine . exps)
  (apply eceval-run machine exps)
  (cdr (assq 'total-pushes (stack-statistics machine))))

(check (eceval-run symbol-operator-eceval factorial '(factorial 5)) => 120)
(check (eceval-run symbol-operator-eceval
                   '(define (twice f) (lambda (x) (f (f x))))
                   '((twice (lambda (x) (* x x))) 3))
       => 81)
(check (eceval-run symbol-operator-eceval '(+)) => 0)

;; All combinations in factorial have symbol operators: four in each of the
;; n - 1 recursive calls, (= n 1) in the last one and the initial call.  Each
;; saves two registers less, so 32n - 16 pushes become 24n - 12.
(define (factorial-pushes machine n)
  (total-pushes machine factorial `(factorial ,n)))

(check (factorial-pushes eceval 5) => (- (* 32 5) 16))
(check (factorial-pushes symbol-operator-eceval 5) => (- (* 24 5) 12))
(check (factorial-pushes symbol-operator-eceval 8) => (- (* 24 8) 12))

;; b. The interpreter would have to discover these special cases every time
;; it evaluates an expression, and each extra test costs time on every
;; evaluation, whether the case applies or not.  The compiler analyzes the
;; program text once, before running it, so all the analysis is paid for
;; once and the generated code contains only the instructions that are
;; needed.  An interpreter can recognize only a few cases cheaply; the
;; compiler can in addition use what it knows about the context, such as
;; which registers later code needs.
