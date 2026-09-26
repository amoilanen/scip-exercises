(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; Rules are applied without copying them.  A rule application creates an
;; environment that owns the rule's variables, and a pattern is always
;; interpreted together with an environment.  Frames bind environment
;; qualified variables, (? id name), to a pattern together with the
;; environment it has to be read in.  The query itself is read in an
;; environment whose variables keep their plain names.
;;
;; Block structure: (local (rule ...) query) makes rules visible in query
;; only.  They are lexically scoped: a variable of a local rule that also
;; occurs in the enclosing rule refers to the enclosing rule's variable.
;;
;; Deduction in a context: (suppose (assertion ...) query) answers query as
;; if the assertions were in the data base.  A supposition must be visible
;; to every rule used in the deduction, not just to the rules written inside
;; the form, so it is scoped dynamically: it travels in the frame.

;;; Environments

(define (make-environment id variables rules parent)
  (list id variables rules parent))

(define (environment-id env) (car env))
(define (environment-variables env) (cadr env))
(define (environment-rules env) (caddr env))
(define (environment-parent env) (cadddr env))

(define global-environment (make-environment #f '() '() #f))

(define (global-environment? env) (eq? env global-environment))

(define (owner-of var env)
  (cond ((global-environment? env) #f)
        ((member var (environment-variables env)) env)
        (else (owner-of var (environment-parent env)))))

(define (visible? var env)
  (if (owner-of var env) #t #f))

(define (variable-key var env)
  (let ((owner (owner-of var env)))
    (cond ((not owner) (error "Variable outside of any rule or query" var))
          ((environment-id owner) (list '? (environment-id owner) (cadr var)))
          (else var))))

;; The variables written in EXP outside the rules of nested local forms.
(define (variables-in exp)
  (let walk ((exp exp) (found '()))
    (cond ((var? exp) (if (member exp found) found (cons exp found)))
          ((local-form? exp) (walk (local-body exp) found))
          ((pair? exp) (walk (cdr exp) (walk (car exp) found)))
          (else found))))

(define (local-form? exp)
  (and (pair? exp) (eq? (car exp) 'local)))
(define (local-body exp) (caddr exp))

(define (visible-rules env)
  (if (global-environment? env)
      '()
      (append (map (lambda (rule) (cons rule env))
                   (environment-rules env))
              (visible-rules (environment-parent env)))))

;;; Frames and unification

(define (make-closure pattern env) (cons pattern env))
(define (closure-pattern closure) (car closure))
(define (closure-env closure) (cdr closure))

;; Follows the bindings of PATTERN as long as it is a bound variable and
;; calls RECEIVE with the pattern and environment reached.
(define (resolve pattern env frame receive)
  (let ((binding (and (var? pattern)
                      (visible? pattern env)
                      (binding-in-frame (variable-key pattern env) frame))))
    (if binding
        (let ((closure (binding-value binding)))
          (resolve (closure-pattern closure) (closure-env closure) frame
                   receive))
        (receive pattern env))))

(define (unify p1 env1 p2 env2 frame)
  (if (eq? frame 'failed)
      'failed
      (resolve
       p1 env1 frame
       (lambda (p1 env1)
         (resolve
          p2 env2 frame
          (lambda (p2 env2)
            (cond ((var? p1) (bind p1 env1 p2 env2 frame))
                  ((var? p2) (bind p2 env2 p1 env1 frame))
                  ((and (pair? p1) (pair? p2))
                   (unify (cdr p1) env1 (cdr p2) env2
                          (unify (car p1) env1 (car p2) env2 frame)))
                  ((equal? p1 p2) frame)
                  (else 'failed))))))))

;; VAR is unbound in FRAME and PATTERN is resolved.
(define (bind var var-env pattern env frame)
  (let ((key (variable-key var var-env)))
    (cond ((and (var? pattern) (equal? key (variable-key pattern env)))
           frame)
          ((occurs? key pattern env frame) 'failed)
          (else (extend key (make-closure pattern env) frame)))))

(define (occurs? key pattern env frame)
  (resolve pattern env frame
           (lambda (pattern env)
             (cond ((var? pattern) (equal? key (variable-key pattern env)))
                   ((pair? pattern)
                    (or (occurs? key (car pattern) env frame)
                        (occurs? key (cdr pattern) env frame)))
                   (else #f)))))

;; Variables that belong to no environment are only met in the text of
;; local rules; they are passed to the handler unqualified.
(define (instantiate-in pattern env frame unbound-var-handler)
  (resolve pattern env frame
           (lambda (pattern env)
             (cond ((var? pattern)
                    (unbound-var-handler
                     (if (visible? pattern env)
                         (variable-key pattern env)
                         pattern)))
                   ((pair? pattern)
                    (cons (instantiate-in (car pattern) env frame
                                          unbound-var-handler)
                          (instantiate-in (cdr pattern) env frame
                                          unbound-var-handler)))
                   (else pattern)))))

;;; The evaluator

(define (qeval query env frame-stream)
  (let ((qproc (get (type query) 'qeval-in-environment)))
    (if qproc
        (qproc (contents query) env frame-stream)
        (simple-query query env frame-stream))))

(define (simple-query pattern env frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed (find-assertions pattern env frame)
                            (delay (apply-rules pattern env frame))))
   frame-stream))

(define (unify-stream p1 env1 p2 env2 frame)
  (let ((result (unify p1 env1 p2 env2 frame)))
    (if (eq? result 'failed)
        the-empty-stream
        (singleton-stream result))))

(define (find-assertions pattern env frame)
  (stream-flatmap
   (lambda (closure)
     (unify-stream pattern env
                   (closure-pattern closure) (closure-env closure)
                   frame))
   (stream-append-delayed
    (list->stream (suppositions frame))
    (delay (stream-map (lambda (assertion)
                         (make-closure assertion global-environment))
                       (fetch-assertions pattern frame))))))

(define (apply-rules pattern env frame)
  (stream-flatmap
   (lambda (rule-and-env)
     (apply-a-rule (car rule-and-env) (cdr rule-and-env) pattern env frame))
   (stream-append-delayed
    (list->stream (visible-rules env))
    (delay (stream-map (lambda (rule) (cons rule global-environment))
                       (fetch-rules pattern frame))))))

(define (apply-a-rule rule definition-env pattern env frame)
  (let* ((own-variables
          (remove (lambda (var) (visible? var definition-env))
                  (variables-in rule)))
         (rule-env (make-environment (new-rule-application-id)
                                     own-variables
                                     '()
                                     definition-env))
         (unified (unify pattern env (conclusion rule) rule-env frame)))
    (if (eq? unified 'failed)
        the-empty-stream
        (qeval (rule-body rule) rule-env (singleton-stream unified)))))

(define (conjoin conjuncts env frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               env
               (qeval (first-conjunct conjuncts) env frame-stream))))

(define (disjoin disjuncts env frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) env frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) env frame-stream)))))

(define (negate operands env frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              env
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (lisp-value call env frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute (instantiate-in call env frame
                                  (lambda (key)
                                    (error "Unknown pat var -- LISP-VALUE"
                                           key))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (always-true ignore env frame-stream) frame-stream)

(define (local-block operands env frame-stream)
  (let ((block-env (make-environment (new-rule-application-id)
                                     '()
                                     (car operands)
                                     env)))
    (qeval (cadr operands) block-env frame-stream)))

(define (suppositions frame)
  (let ((binding (binding-in-frame 'suppositions frame)))
    (if binding (binding-value binding) '())))

(define (with-suppositions closures frame)
  (extend 'suppositions closures frame))

(define (suppose operands env frame-stream)
  (let ((assertions (map (lambda (assertion) (make-closure assertion env))
                         (car operands)))
        (query (cadr operands)))
    (stream-flatmap
     (lambda (frame)
       (let ((outer (suppositions frame)))
         (stream-map
          (lambda (result) (with-suppositions outer result))
          (qeval query
                 env
                 (singleton-stream
                  (with-suppositions (append assertions outer) frame))))))
     frame-stream)))

(put 'and 'qeval-in-environment conjoin)
(put 'or 'qeval-in-environment disjoin)
(put 'not 'qeval-in-environment negate)
(put 'lisp-value 'qeval-in-environment lisp-value)
(put 'always-true 'qeval-in-environment always-true)
(put 'local 'qeval-in-environment local-block)
(put 'suppose 'qeval-in-environment suppose)

(define (query-results query)
  (let* ((processed (query-syntax-process query))
         (env (make-environment #f
                                (variables-in processed)
                                '()
                                global-environment)))
    (stream-map (lambda (frame)
                  (instantiate-in processed env frame contract-question-mark))
                (qeval processed env (singleton-stream empty-frame)))))

(initialize-data-base!
 (append microshaft-data-base
         append-to-form-rules
         '((rule (reports-up-to ?person ?boss)
                 (local ((rule (climb ?x)
                               (supervisor ?x ?boss))
                         (rule (climb ?x)
                               (and (supervisor ?x ?middle)
                                    (climb ?middle))))
                   (climb ?person))))))

(check (run-query '(lives-near ?x (Bitdiddle Ben)))
       (=> same-elements?)
       '((lives-near (Reasoner Louis) (Bitdiddle Ben))
         (lives-near (Aull DeWitt) (Bitdiddle Ben))))

(check (run-query '(outranked-by (Reasoner Louis) ?who))
       (=> same-elements?)
       '((outranked-by (Reasoner Louis) (Hacker Alyssa P))
         (outranked-by (Reasoner Louis) (Bitdiddle Ben))
         (outranked-by (Reasoner Louis) (Warbucks Oliver))))

(check (run-query '(append-to-form ?x ?y (a b)))
       (=> same-elements?)
       '((append-to-form () (a b) (a b))
         (append-to-form (a) (b) (a b))
         (append-to-form (a b) () (a b))))

(check (run-query '(and (salary ?person ?amount) (lisp-value > ?amount 70000)))
       (=> same-elements?)
       '((and (salary (Warbucks Oliver) 150000) (lisp-value > 150000 70000))
         (and (salary (Scrooge Eben) 75000) (lisp-value > 75000 70000))))

;; climb is recursive, is visible only inside reports-up-to, and refers to
;; the ?boss of the reports-up-to application that defines it.
(check (run-query '(reports-up-to ?who (Bitdiddle Ben)))
       (=> same-elements?)
       '((reports-up-to (Hacker Alyssa P) (Bitdiddle Ben))
         (reports-up-to (Fect Cy D) (Bitdiddle Ben))
         (reports-up-to (Tweakit Lem E) (Bitdiddle Ben))
         (reports-up-to (Reasoner Louis) (Bitdiddle Ben))))

(check (run-query '(reports-up-to (Reasoner Louis) ?boss))
       (=> same-elements?)
       '((reports-up-to (Reasoner Louis) (Hacker Alyssa P))
         (reports-up-to (Reasoner Louis) (Bitdiddle Ben))
         (reports-up-to (Reasoner Louis) (Warbucks Oliver))))

(check (run-query '(climb ?x)) => '())

(define (last-conjunct answer) (last (local-body answer)))

(check (map last-conjunct
            (run-query '(local ((rule (colleague ?x)
                                      (and (supervisor ?x ?boss)
                                           (not (same ?x ?me)))))
                          (and (same ?me (Hacker Alyssa P))
                               (supervisor ?me ?boss)
                               (colleague ?who)))))
       (=> same-elements?)
       '((colleague (Fect Cy D)) (colleague (Tweakit Lem E))))

(check (run-query '(local ((rule (colleague ?x)
                                 (supervisor ?x (Scrooge Eben))))
                     (colleague ?who)))
       => '((local ((rule (colleague ?x) (supervisor ?x (Scrooge Eben))))
              (colleague (Cratchet Robert)))))

;; The supposition is used by outranked-by, a rule defined outside the
;; suppose form, and is gone again for the conjuncts after it.
(define (supposed-boss answer) (caddr (caddr answer)))

(check (map supposed-boss
            (run-query '(suppose ((supervisor (Reasoner Louis) (Fect Cy D)))
                          (outranked-by (Reasoner Louis) ?who))))
       (=> same-elements?)
       '((Hacker Alyssa P) (Bitdiddle Ben) (Warbucks Oliver)
         (Fect Cy D) (Bitdiddle Ben) (Warbucks Oliver)))

(check (run-query '(and (suppose ((job (Reasoner Louis) (computer wizard)))
                          (job ?who (computer wizard)))
                        (job ?who (computer . ?actual))))
       (=> same-elements?)
       '((and (suppose ((job (Reasoner Louis) (computer wizard)))
                (job (Bitdiddle Ben) (computer wizard)))
              (job (Bitdiddle Ben) (computer wizard)))
         (and (suppose ((job (Reasoner Louis) (computer wizard)))
                (job (Reasoner Louis) (computer wizard)))
              (job (Reasoner Louis) (computer programmer trainee)))))
