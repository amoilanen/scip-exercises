;; The analyzing evaluator of section 4.1.7, built on the syntax procedures
;; and environment operations of mceval.scm.
;;
;;   (analyzing-eval exp env)    the book's eval: ((analyze exp) env)
;;   (analyzing-interpret exp ...)  like interpret, using analyzing-eval

(load (merge-pathnames "mceval.scm" (current-load-pathname)))

(define (analyzing-eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((text (text-of-quotation exp)))
    (lambda (env) text)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (value-proc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (value-proc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (value-proc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (value-proc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((predicate-proc (analyze (if-predicate exp)))
        (consequent-proc (analyze (if-consequent exp)))
        (alternative-proc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (predicate-proc env))
          (consequent-proc env)
          (alternative-proc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (body-proc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars body-proc env))))

(define (analyze-sequence exps)
  (define (sequentially first-proc second-proc)
    (lambda (env) (first-proc env) (second-proc env)))
  (define (chain first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (chain (sequentially first-proc (car rest-procs))
               (cdr rest-procs))))
  (if (null? exps)
      (error "Empty sequence -- ANALYZE"))
  (let ((procs (map analyze exps)))
    (chain (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((operator-proc (analyze (operator exp)))
        (operand-procs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (operator-proc env)
       (map (lambda (operand-proc) (operand-proc env))
            operand-procs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (analyzing-interpret . exps)
  (let ((env (setup-environment)))
    (let loop ((exps exps) (value 'ok))
      (if (null? exps)
          value
          (loop (cdr exps) (analyzing-eval (car exps) env))))))
