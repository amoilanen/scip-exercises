(load "lib/check.scm")
(load "ch5/lib/eceval.scm")

;; Operations that can fail return an eceval-error instead of signalling an
;; error in the underlying Scheme.  The controller tests for it after each
;; such operation and branches to signal-error, which ends the evaluation
;; with the error in val; the next run starts with a fresh stack.

(define-record-type eceval-error
    (make-eceval-error message irritants)
    eceval-error?
  (message eceval-error-message)
  (irritants eceval-error-irritants))

(define (eceval-error message . irritants)
  (make-eceval-error message irritants))

;;; a. Unbound variables and wrong numbers of arguments

;; Returns the list whose car holds the value of var, or #f if var is unbound.
(define (binding-cell var env)
  (if (eq? env the-empty-environment)
      #f
      (let ((frame (first-frame env)))
        (let scan ((vars (frame-variables frame))
                   (vals (frame-values frame)))
          (cond ((null? vars) (binding-cell var (enclosing-environment env)))
                ((eq? var (car vars)) vals)
                (else (scan (cdr vars) (cdr vals))))))))

(define (checked-lookup-variable-value var env)
  (let ((cell (binding-cell var env)))
    (if cell
        (car cell)
        (eceval-error "Unbound variable" var))))

(define (checked-set-variable-value! var val env)
  (let ((cell (binding-cell var env)))
    (if cell
        (begin (set-car! cell val) 'ok)
        (eceval-error "Unbound variable -- SET!" var))))

(define (checked-extend-environment vars vals base-env)
  (let ((variable-count (length vars))
        (value-count (length vals)))
    (cond ((< variable-count value-count)
           (eceval-error "Too many arguments supplied" vars vals))
          ((> variable-count value-count)
           (eceval-error "Too few arguments supplied" vars vals))
          (else (extend-environment vars vals base-env)))))

;;; b. Errors in primitives

;; A check takes the argument list and returns an error message, or #f if
;; the primitive can be applied safely.

(define (arguments count predicate)
  (lambda (args)
    (cond ((not (= (length args) count)) "Wrong number of arguments")
          ((not (every predicate args)) "Wrong type argument")
          (else #f))))

(define (numbers-at-least count)
  (lambda (args)
    (cond ((< (length args) count) "Too few arguments")
          ((not (every number? args)) "Wrong type argument")
          (else #f))))

(define (any-value x) #t)

(define (list-of-at-least-two? x)
  (and (pair? x) (pair? (cdr x))))

(define (check-division args)
  (or ((numbers-at-least 1) args)
      (and (any zero? (if (null? (cdr args)) args (cdr args)))
           "Division by zero")))

(define (check-remainder args)
  (or ((arguments 2 integer?) args)
      (and (zero? (cadr args)) "Division by zero")))

(define primitive-checks
  (list (list car (arguments 1 pair?))
        (list cdr (arguments 1 pair?))
        (list cadr (arguments 1 list-of-at-least-two?))
        (list cddr (arguments 1 list-of-at-least-two?))
        (list cons (arguments 2 any-value))
        (list list (lambda (args) #f))
        (list null? (arguments 1 any-value))
        (list pair? (arguments 1 any-value))
        (list number? (arguments 1 any-value))
        (list symbol? (arguments 1 any-value))
        (list eq? (arguments 2 any-value))
        (list equal? (arguments 2 any-value))
        (list not (arguments 1 any-value))
        (list + (numbers-at-least 0))
        (list * (numbers-at-least 0))
        (list - (numbers-at-least 1))
        (list / check-division)
        (list = (numbers-at-least 1))
        (list < (numbers-at-least 1))
        (list > (numbers-at-least 1))
        (list <= (numbers-at-least 1))
        (list >= (numbers-at-least 1))
        (list remainder check-remainder)
        (list abs (arguments 1 real?))
        (list display (arguments 1 any-value))
        (list newline (arguments 0 any-value))))

(define (checked-apply-primitive-procedure proc args)
  (let* ((implementation (primitive-implementation proc))
         (entry (assq implementation primitive-checks))
         (problem (and entry ((cadr entry) args))))
    (if problem
        (apply eceval-error problem args)
        (apply implementation args))))

;;; Controller

(define checked-variable-code
  '(ev-checked-variable
    (assign val (op checked-lookup-variable-value) (reg exp) (reg env))
    (test (op eceval-error?) (reg val))
    (branch (label signal-error))
    (goto (reg continue))
    ev-checked-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-checked-assignment-1))
    (goto (label eval-dispatch))
    ev-checked-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (assign val (op checked-set-variable-value!)
            (reg unev) (reg val) (reg env))
    (test (op eceval-error?) (reg val))
    (branch (label signal-error))
    (goto (reg continue))))

(define apply-dispatch-code
  '(apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))
    primitive-apply
    (assign val (op checked-apply-primitive-procedure)
            (reg proc) (reg argl))
    (test (op eceval-error?) (reg val))
    (branch (label signal-error))
    (restore continue)
    (goto (reg continue))
    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign val (op checked-extend-environment)
            (reg unev) (reg argl) (reg env))
    (test (op eceval-error?) (reg val))
    (branch (label signal-error))
    (assign env (reg val))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))))

(define error-code
  '(unknown-expression-type
    (assign val (op eceval-error)
            (const "Unknown expression type") (reg exp))
    (goto (label signal-error))
    unknown-procedure-type
    (assign val (op eceval-error)
            (const "Unknown procedure type") (reg proc))
    signal-error
    (goto (label done))))

(define checked-dispatch-table
  (map (lambda (entry)
         (case (car entry)
           ((variable?) '(variable? ev-checked-variable))
           ((assignment?) '(assignment? ev-checked-assignment))
           (else entry)))
       eceval-dispatch-table))

(define checked-eceval
  (make-eceval
   checked-dispatch-table
   checked-variable-code
   (operation-entries
    'eceval-error eceval-error
    'eceval-error? eceval-error?
    'checked-lookup-variable-value checked-lookup-variable-value
    'checked-set-variable-value! checked-set-variable-value!
    'checked-extend-environment checked-extend-environment
    'checked-apply-primitive-procedure checked-apply-primitive-procedure)))

(define (run . exps)
  (let ((value (apply eceval-run checked-eceval exps)))
    (if (eceval-error? value)
        (cons (eceval-error-message value) (eceval-error-irritants value))
        value)))

(check (run '(define (add a b) (+ a b))
            '(define x 1)
            '(set! x (add x 2))
            'x)
       => 3)

(check (run 'x) => '("Unbound variable" x))
(check (run '(set! x 1)) => '("Unbound variable -- SET!" x))
(check (run '(define (f) (g)) '(f)) => '("Unbound variable" g))
(check (run '((lambda (x) x) 1 2))
       => '("Too many arguments supplied" (x) (1 2)))
(check (run '((lambda (x y) x) 1))
       => '("Too few arguments supplied" (x y) (1)))
(check (run '(1 2)) => '("Unknown procedure type" 1))
(check (run #t) => '("Unknown expression type" #t))

(check (run '(car 'a)) => '("Wrong type argument" a))
(check (run '(cadr '(1))) => '("Wrong type argument" (1)))
(check (run '(car '(1) '(2))) => '("Wrong number of arguments" (1) (2)))
(check (run '(+ 1 'a)) => '("Wrong type argument" 1 a))
(check (run '(-)) => '("Too few arguments"))
(check (run '(/ 1 0)) => '("Division by zero" 1 0))
(check (run '(/ 0)) => '("Division by zero" 0))
(check (run '(remainder 7 0)) => '("Division by zero" 7 0))
(check (run '(list (/ 6 3) (remainder 7 2) (car '(1)) (- 5))) => '(2 1 1 -5))

(check (run '(define (f n)
               (if (= n 0)
                   (car '())
                   (+ 1 (f (- n 1)))))
            '(f 5))
       => '("Wrong type argument" ()))

;; An error ends only the expression it occurs in; the machine goes on.
(check (run '(define y (car 'a)) '(+ 1 2)) => 3)
(check (run '(* 6 7)) => 42)
