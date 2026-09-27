;; The nondeterministic evaluator of section 4.3.3: an analyzing evaluator
;; whose execution procedures take success and failure continuations.
;; Syntax and environment procedures come from the metacircular evaluator.
;;
;; Special forms are dispatched through a table, so an exercise adds one with
;; (install-special-form! tag analyzer) instead of rewriting analyze.

(load (merge-pathnames "mceval.scm" (current-load-pathname)))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define special-forms (make-strong-eqv-hash-table))

(define (install-special-form! tag analyzer)
  (hash-table-set! special-forms tag analyzer))

(define (special-form-analyzer exp)
  (and (pair? exp)
       (hash-table-ref/default special-forms (car exp) #f)))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((special-form-analyzer exp)
         => (lambda (analyze-form) (analyze-form exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type -- ANALYZE" exp))))

;;; Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((text (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed text fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((parameters (lambda-parameters exp))
        (body (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure parameters body env) fail))))

;;; Conditionals and sequences

(define (analyze-if exp)
  (let ((predicate (analyze (if-predicate exp)))
        (consequent (analyze (if-consequent exp)))
        (alternative (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (predicate env
                 (lambda (value fail2)
                   (if (true? value)
                       (consequent env succeed fail2)
                       (alternative env succeed fail2)))
                 fail))))

(define (analyze-sequence exps)
  (define (sequentially first rest)
    (lambda (env succeed fail)
      (first env
             (lambda (ignored fail2)
               (rest env succeed fail2))
             fail)))
  (if (null? exps)
      (error "Empty sequence -- ANALYZE"))
  (let loop ((procs (map analyze exps)))
    (if (null? (cdr procs))
        (car procs)
        (sequentially (car procs) (loop (cdr procs))))))

(define (analyze-begin exp)
  (analyze-sequence (begin-actions exp)))

(define (analyze-cond exp)
  (analyze (cond->if exp)))

(define (analyze-let exp)
  (analyze (let->combination exp)))

(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (map car (let-bindings exp)) (let-body exp))
        (map cadr (let-bindings exp))))

(define (analyze-let* exp)
  (analyze (let*->nested-lets exp)))

(define (let*->nested-lets exp)
  (let nest ((bindings (let-bindings exp)))
    (if (or (null? bindings) (null? (cdr bindings)))
        (cons 'let (cons bindings (let-body exp)))
        (list 'let (list (car bindings)) (nest (cdr bindings))))))

;; and/or short-circuit: an operand is evaluated only if it is needed.
(define (analyze-and exp)
  (let loop ((procs (map analyze (cdr exp))))
    (cond ((null? procs)
           (lambda (env succeed fail) (succeed true fail)))
          ((null? (cdr procs)) (car procs))
          (else
           (let ((first (car procs))
                 (rest (loop (cdr procs))))
             (lambda (env succeed fail)
               (first env
                      (lambda (value fail2)
                        (if (true? value)
                            (rest env succeed fail2)
                            (succeed value fail2)))
                      fail)))))))

(define (analyze-or exp)
  (let loop ((procs (map analyze (cdr exp))))
    (if (null? procs)
        (lambda (env succeed fail) (succeed false fail))
        (let ((first (car procs))
              (rest (loop (cdr procs))))
          (lambda (env succeed fail)
            (first env
                   (lambda (value fail2)
                     (if (true? value)
                         (succeed value fail2)
                         (rest env succeed fail2)))
                   fail))))))

;;; Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (value-proc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (value-proc env
                  (lambda (value fail2)
                    (define-variable! var value env)
                    (succeed 'ok fail2))
                  fail))))

;; On backtracking the old value is restored before the failure propagates.
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (value-proc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (value-proc env
                  (lambda (value fail2)
                    (let ((old-value (lookup-variable-value var env)))
                      (set-variable-value! var value env)
                      (succeed 'ok
                               (lambda ()
                                 (set-variable-value! var old-value env)
                                 (fail2)))))
                  fail))))

;;; Applications

(define (analyze-application exp)
  (let ((operator-proc (analyze (operator exp)))
        (operand-procs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (operator-proc env
                     (lambda (proc fail2)
                       (get-args operand-procs
                                 env
                                 (lambda (args fail3)
                                   (execute-application
                                    proc args succeed fail3))
                                 fail2))
                     fail))))

;; Operands are evaluated from left to right.
(define (get-args operand-procs env succeed fail)
  (if (null? operand-procs)
      (succeed '() fail)
      ((car operand-procs)
       env
       (lambda (arg fail2)
         (get-args (cdr operand-procs)
                   env
                   (lambda (args fail3)
                     (succeed (cons arg args) fail3))
                   fail2))
       fail)))

(define application-count 0)

(define (execute-application proc args succeed fail)
  (set! application-count (+ application-count 1))
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

;;; amb

(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((choice-procs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (let try-next ((choices choice-procs))
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices)))))))))

(install-special-form! 'quote analyze-quoted)
(install-special-form! 'set! analyze-assignment)
(install-special-form! 'define analyze-definition)
(install-special-form! 'if analyze-if)
(install-special-form! 'lambda analyze-lambda)
(install-special-form! 'begin analyze-begin)
(install-special-form! 'cond analyze-cond)
(install-special-form! 'let analyze-let)
(install-special-form! 'let* analyze-let*)
(install-special-form! 'and analyze-and)
(install-special-form! 'or analyze-or)
(install-special-form! 'amb analyze-amb)

;;; The global environment

;; The book's lookup scans a frame with an interpreted loop, and looking up
;; primitives in the large global frame dominates the running time of the
;; puzzles; scanning with the built-in memq makes them about twice as fast.
(define (lookup-variable-value var env)
  (let env-loop ((env env))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let* ((frame (first-frame env))
               (vars (frame-variables frame))
               (tail (memq var vars)))
          (if tail
              (list-ref (frame-values frame)
                        (- (length vars) (length tail)))
              (env-loop (enclosing-environment env)))))))

(define primitive-procedures
  (append primitive-procedures
          (list (list 'reverse reverse)
                (list 'memq memq)
                (list 'assq assq)
                (list 'member member)
                (list 'even? even?)
                (list 'integer? integer?)
                (list 'sqrt sqrt)
                (list 'square square))))

(define amb-prelude
  '((define (require p)
      (if (not p) (amb)))
    (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))
    (define (an-integer-starting-from n)
      (amb n (an-integer-starting-from (+ n 1))))
    (define (distinct? items)
      (cond ((null? items) true)
            ((null? (cdr items)) true)
            ((member (car items) (cdr items)) false)
            (else (distinct? (cdr items)))))))

;;; Running programs non-interactively

(define (amb-define! env exp)
  (ambeval exp env
           (lambda (value fail) value)
           (lambda () (error "Definition has no value -- AMB-DEFINE!" exp))))

;; A fresh global environment holding the prelude and the given definitions.
(define (amb-environment . definitions)
  (let ((env (setup-environment)))
    (for-each (lambda (exp) (amb-define! env exp))
              (append amb-prelude definitions))
    env))

;; The values of exp in the order amb finds them: all of them, or at most
;; limit of them.
(define (amb-collect exp env #!optional limit)
  (let ((results '())
        (found 0))
    (ambeval exp env
             (lambda (value next)
               (set! results (cons value results))
               (set! found (+ found 1))
               (if (or (default-object? limit) (< found limit))
                   (next)))
             (lambda () 'no-more-values))
    (reverse results)))

;; The number of procedure applications performed by thunk.
(define (count-applications thunk)
  (let ((before application-count))
    (thunk)
    (- application-count before)))

;; The value of thunk, or the symbol out-of-budget as soon as thunk has made
;; more than budget applications: a way to observe a search that never ends.
(define (with-application-budget budget thunk)
  (call-with-current-continuation
   (lambda (return)
     (let ((limit (+ application-count budget))
           (execute execute-application))
       (fluid-let ((execute-application
                    (lambda (proc args succeed fail)
                      (if (> application-count limit)
                          (return 'out-of-budget))
                      (execute proc args succeed fail))))
         (thunk))))))

;;; Programs of section 4.3.2 used by several exercises

(define multiple-dwelling-program
  '((define (multiple-dwelling)
      (let ((baker (amb 1 2 3 4 5))
            (cooper (amb 1 2 3 4 5))
            (fletcher (amb 1 2 3 4 5))
            (miller (amb 1 2 3 4 5))
            (smith (amb 1 2 3 4 5)))
        (require (distinct? (list baker cooper fletcher miller smith)))
        (require (not (= baker 5)))
        (require (not (= cooper 1)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (> miller cooper))
        (require (not (= (abs (- smith fletcher)) 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))))

(define parser-program
  '((define nouns '(noun student professor cat class))
    (define verbs '(verb studies lectures eats sleeps))
    (define articles '(article the a))
    (define prepositions '(prep for to in by with))

    (define *unparsed* '())

    (define (parse input)
      (set! *unparsed* input)
      (let ((sentence (parse-sentence)))
        (require (null? *unparsed*))
        sentence))

    (define (parse-word word-list)
      (require (not (null? *unparsed*)))
      (require (memq (car *unparsed*) (cdr word-list)))
      (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car word-list) found-word)))

    (define (parse-sentence)
      (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

    (define (parse-simple-noun-phrase)
      (list 'simple-noun-phrase
            (parse-word articles)
            (parse-word nouns)))

    (define (parse-noun-phrase)
      (define (maybe-extend noun-phrase)
        (amb noun-phrase
             (maybe-extend (list 'noun-phrase
                                 noun-phrase
                                 (parse-prepositional-phrase)))))
      (maybe-extend (parse-simple-noun-phrase)))

    (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
        (amb verb-phrase
             (maybe-extend (list 'verb-phrase
                                 verb-phrase
                                 (parse-prepositional-phrase)))))
      (maybe-extend (parse-word verbs)))

    (define (parse-prepositional-phrase)
      (list 'prep-phrase
            (parse-word prepositions)
            (parse-noun-phrase)))))
