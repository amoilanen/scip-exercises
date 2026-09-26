;; The explicit-control evaluator (section 5.4) running on the simulator.
;;
;; The controller is assembled from sections so that exercises can add special
;; forms without copying it: a special form is a dispatch entry
;; (predicate label) plus the code for its label.
;;
;;   (eceval-eval exp)  evaluates exp in the-global-environment
;;   (make-eceval dispatch-table extra-code extra-operations)  builds a variant

(load "ch4/lib/mceval.scm")
(load "ch5/lib/regsim.scm")

;;; Operations

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))
(define (no-more-exps? seq) (null? seq))
(define (get-global-environment) the-global-environment)

(define (signal-eceval-error message irritant)
  (error message irritant))

(define (operation-entries . procedures-and-names)
  (let loop ((items procedures-and-names) (entries '()))
    (if (null? items)
        (reverse entries)
        (loop (cddr items)
              (cons (list (car items) (cadr items)) entries)))))

(define eceval-operations
  (operation-entries
   'self-evaluating? self-evaluating?
   'variable? variable?
   'quoted? quoted?
   'text-of-quotation text-of-quotation
   'assignment? assignment?
   'assignment-variable assignment-variable
   'assignment-value assignment-value
   'definition? definition?
   'definition-variable definition-variable
   'definition-value definition-value
   'if? if?
   'if-predicate if-predicate
   'if-consequent if-consequent
   'if-alternative if-alternative
   'lambda? lambda?
   'lambda-parameters lambda-parameters
   'lambda-body lambda-body
   'begin? begin?
   'begin-actions begin-actions
   'first-exp first-exp
   'last-exp? last-exp?
   'rest-exps rest-exps
   'no-more-exps? no-more-exps?
   'application? application?
   'operator operator
   'operands operands
   'no-operands? no-operands?
   'first-operand first-operand
   'rest-operands rest-operands
   'last-operand? last-operand?
   'empty-arglist empty-arglist
   'adjoin-arg adjoin-arg
   'true? true?
   'false? false?
   'make-procedure make-procedure
   'compound-procedure? compound-procedure?
   'procedure-parameters procedure-parameters
   'procedure-body procedure-body
   'procedure-environment procedure-environment
   'primitive-procedure? primitive-procedure?
   'apply-primitive-procedure apply-primitive-procedure
   'extend-environment extend-environment
   'lookup-variable-value lookup-variable-value
   'set-variable-value! set-variable-value!
   'define-variable! define-variable!
   'get-global-environment get-global-environment
   'signal-eceval-error signal-eceval-error))

(define eceval-registers '(exp env val proc argl continue unev))

;;; Controller

;; Applications are tried after every entry of the dispatch table.
(define eceval-dispatch-table
  '((self-evaluating? ev-self-eval)
    (variable? ev-variable)
    (quoted? ev-quoted)
    (assignment? ev-assignment)
    (definition? ev-definition)
    (if? ev-if)
    (lambda? ev-lambda)
    (begin? ev-begin)))

(define (eval-dispatch-code dispatch-table)
  (append
   '(eval-dispatch)
   (append-map (lambda (entry)
                 `((test (op ,(car entry)) (reg exp))
                   (branch (label ,(cadr entry)))))
               dispatch-table)
   '((test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type)))))

(define simple-expressions-code
  '(ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
    ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (goto (reg continue))))

(define application-code
  '(ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))
    ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))))

(define apply-dispatch-code
  '(apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))
    primitive-apply
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (restore continue)
    (goto (reg continue))
    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))))

;; ev-sequence expects the caller's continuation on the stack; the last
;; expression is evaluated without saving anything, which makes the
;; evaluator tail-recursive.
(define sequence-code
  '(ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))
    ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
    ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))))

(define conditional-code
  '(ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))
    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))))

(define assignment-code
  '(ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))
    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))
    ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))
    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))))

(define error-code
  '(unknown-expression-type
    (perform (op signal-eceval-error)
             (const "Unknown expression type -- ECEVAL") (reg exp))
    unknown-procedure-type
    (perform (op signal-eceval-error)
             (const "Unknown procedure type -- ECEVAL") (reg proc))))

(define (eceval-controller dispatch-table extra-code)
  (append
   '((perform (op initialize-stack))
     (assign continue (label done))
     (goto (label eval-dispatch)))
   (eval-dispatch-code dispatch-table)
   simple-expressions-code
   application-code
   apply-dispatch-code
   sequence-code
   conditional-code
   assignment-code
   extra-code
   error-code
   '(done)))

(define (make-eceval dispatch-table extra-code extra-operations)
  (make-machine eceval-registers
                (append eceval-operations extra-operations)
                (eceval-controller dispatch-table extra-code)))

(define eceval (make-eceval eceval-dispatch-table '() '()))

;; Evaluates the expressions one by one in a fresh global environment and
;; returns the value of the last one.  Afterwards (stack-statistics machine)
;; describes the evaluation of the last expression.
(define (eceval-run machine . exps)
  (set! the-global-environment (setup-environment))
  (let loop ((exps exps) (value 'ok))
    (if (null? exps)
        value
        (begin
          (set-register-contents! machine 'exp (car exps))
          (set-register-contents! machine 'env the-global-environment)
          (start machine)
          (loop (cdr exps) (get-register-contents machine 'val))))))
