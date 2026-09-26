(load "lib/check.scm")
(load "ch5/lib/eceval.scm")

;;; Thunks, memoized as in section 4.2.2

(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (set-thunk-value! thunk value)
  (set-car! thunk 'evaluated-thunk)
  (set-car! (cdr thunk) value)
  (set-cdr! (cdr thunk) '()))

(define (delay-operands exps env)
  (map (lambda (exp) (delay-it exp env)) exps))

;;; Controller

;; actual-value evaluates exp in env and forces the result.  force-it
;; expects the value in val and the caller's continuation on the stack.
(define forcing-code
  '(actual-value
    (save continue)
    (assign continue (label force-it))
    (goto (label eval-dispatch))
    force-it
    (test (op thunk?) (reg val))
    (branch (label force-thunk))
    (test (op evaluated-thunk?) (reg val))
    (branch (label force-evaluated-thunk))
    (restore continue)
    (goto (reg continue))
    force-thunk
    (save val)
    (assign exp (op thunk-exp) (reg val))
    (assign env (op thunk-env) (reg val))
    (assign continue (label force-thunk-done))
    (goto (label actual-value))
    force-thunk-done
    (restore exp)
    (perform (op set-thunk-value!) (reg exp) (reg val))
    (restore continue)
    (goto (reg continue))
    force-evaluated-thunk
    (assign val (op thunk-value) (reg val))
    (restore continue)
    (goto (reg continue))))

;; The operator is forced; operands are forced for primitives only, and
;; passed to compound procedures as thunks.
(define lazy-application-code
  '(ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label actual-value))
    ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign proc (reg val))
    (test (op compound-procedure?) (reg proc))
    (branch (label ev-appl-delay-operands))
    (assign argl (op empty-arglist))
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
    (goto (label actual-value))
    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label actual-value))
    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))
    ev-appl-delay-operands
    (assign argl (op delay-operands) (reg unev) (reg env))
    (goto (label apply-dispatch))))

(define lazy-conditional-code
  '(ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label actual-value))
    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))))

;; Like the driver loop of section 4.2, the machine forces the value of the
;; whole expression, so it starts at actual-value rather than eval-dispatch.
(define lazy-eceval
  (make-machine
   eceval-registers
   (append eceval-operations
           (operation-entries 'thunk? thunk?
                              'thunk-exp thunk-exp
                              'thunk-env thunk-env
                              'evaluated-thunk? evaluated-thunk?
                              'thunk-value thunk-value
                              'set-thunk-value! set-thunk-value!
                              'delay-operands delay-operands))
   (append '((perform (op initialize-stack))
             (assign continue (label done))
             (goto (label actual-value)))
           (eval-dispatch-code eceval-dispatch-table)
           simple-expressions-code
           lazy-application-code
           apply-dispatch-code
           sequence-code
           lazy-conditional-code
           assignment-code
           forcing-code
           error-code
           '(done))))

(define (run . exps)
  (apply eceval-run lazy-eceval exps))

(check (run '(define (try a b) (if (= a 0) 1 b))
            '(try 0 (/ 1 0)))
       => 1)

(check (run '(define (unless condition usual exceptional)
               (if condition exceptional usual))
            '(define (divide a b)
               (unless (= b 0) (/ a b) 'division-by-zero))
            '(list (divide 6 3) (divide 1 0)))
       => '(2 division-by-zero))

(check (run '(define (loop) (loop))
            '(define (first a b) a)
            '(first 'done (loop)))
       => 'done)

(check (run '(define (id x) x)
            '(id (id (+ 1 2))))
       => 3)

(check (run '(define (compose f g) (lambda (x) (f (g x))))
            '((compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 4))
       => 25)

(check (run '(define (factorial n)
               (if (= n 0) 1 (* n (factorial (- n 1)))))
            '(factorial 6))
       => 720)

;; The argument of id is forced once, when w is first used; afterwards the
;; memoized value is returned without evaluating (id 10) again.
(check (run '(define count 0)
            '(define (id x) (set! count (+ count 1)) x)
            '(define w (id (id 10)))
            '(list count w count w count))
       => '(1 10 2 10 2))
