(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/query.scm" (current-load-pathname)))
(load (merge-pathnames "lib/amb.scm" (current-load-pathname)))

;; The query evaluator as a program for the amb evaluator: qeval takes one
;; frame and returns one extended frame, choosing an assertion, a rule or a
;; disjunct with amb, and backtracks for the other answers.  The syntax
;; processing, the indexed data base of the stream implementation and
;; instantiating the answers stay in the underlying Scheme.
;;
;; not needs to know that a query has no answer at all, which a
;; nondeterministic program cannot find out about itself, so the amb
;; evaluator gets one special form: (succeeds? exp) is true if exp has a
;; value, false otherwise, and leaves no choice points behind.

(define (analyze-succeeds? exp)
  (let ((proc (analyze (cadr exp))))
    (lambda (env succeed fail)
      (proc env
            (lambda (value fail2) (succeed true fail))
            (lambda () (succeed false fail))))))

(install-special-form! 'succeeds? analyze-succeeds?)

(define (candidates fetch)
  (lambda (pattern)
    (stream->list (fetch pattern empty-frame))))

(define primitive-procedures
  (append primitive-procedures
          (list (list 'assoc assoc)
                (list 'execute execute)
                (list 'error error)
                (list 'assertions-for (candidates fetch-assertions))
                (list 'rules-for (candidates fetch-rules)))))

(define query-program
  '((define (qeval query frame)
      (let ((type (car query)))
        (cond ((eq? type 'and) (conjoin (cdr query) frame))
              ((eq? type 'or) (qeval (an-element-of (cdr query)) frame))
              ((eq? type 'not) (negate (cadr query) frame))
              ((eq? type 'lisp-value) (lisp-value (cdr query) frame))
              ((eq? type 'always-true) frame)
              (else (simple-query query frame)))))

    (define (conjoin conjuncts frame)
      (if (null? conjuncts)
          frame
          (conjoin (cdr conjuncts) (qeval (car conjuncts) frame))))

    (define (negate query frame)
      (require (not (succeeds? (qeval query frame))))
      frame)

    (define (lisp-value call frame)
      (require (execute (instantiate call frame)))
      frame)

    (define (simple-query pattern frame)
      (amb (find-assertion pattern frame)
           (apply-a-rule (an-element-of (rules-for pattern)) pattern frame)))

    (define (find-assertion pattern frame)
      (unless-failed
       (pattern-match pattern (an-element-of (assertions-for pattern)) frame)))

    (define (apply-a-rule rule pattern frame)
      (let ((clean-rule (rename-variables-in rule)))
        (qeval (rule-body clean-rule)
               (unless-failed
                (unify-match pattern (conclusion clean-rule) frame)))))

    (define (unless-failed frame)
      (require (not (eq? frame 'failed)))
      frame)

    (define (conclusion rule) (cadr rule))

    (define (rule-body rule)
      (if (null? (cddr rule))
          '(always-true)
          (car (cddr rule))))

    (define (var? exp)
      (and (pair? exp) (eq? (car exp) '?)))

    (define (binding-in-frame var frame) (assoc var frame))

    (define (extend var value frame) (cons (cons var value) frame))

    (define (pattern-match pattern datum frame)
      (cond ((eq? frame 'failed) 'failed)
            ((equal? pattern datum) frame)
            ((var? pattern) (extend-if-consistent pattern datum frame))
            ((and (pair? pattern) (pair? datum))
             (pattern-match (cdr pattern)
                            (cdr datum)
                            (pattern-match (car pattern) (car datum) frame)))
            (else 'failed)))

    (define (extend-if-consistent var datum frame)
      (let ((binding (binding-in-frame var frame)))
        (if binding
            (pattern-match (cdr binding) datum frame)
            (extend var datum frame))))

    (define (unify-match p1 p2 frame)
      (cond ((eq? frame 'failed) 'failed)
            ((equal? p1 p2) frame)
            ((var? p1) (extend-if-possible p1 p2 frame))
            ((var? p2) (extend-if-possible p2 p1 frame))
            ((and (pair? p1) (pair? p2))
             (unify-match (cdr p1)
                          (cdr p2)
                          (unify-match (car p1) (car p2) frame)))
            (else 'failed)))

    (define (extend-if-possible var value frame)
      (let ((binding (binding-in-frame var frame)))
        (cond (binding (unify-match (cdr binding) value frame))
              ((and (var? value) (binding-in-frame value frame))
               (unify-match var (cdr (binding-in-frame value frame)) frame))
              ((depends-on? value var frame) 'failed)
              (else (extend var value frame)))))

    (define (depends-on? exp var frame)
      (cond ((var? exp)
             (or (equal? var exp)
                 (let ((binding (binding-in-frame exp frame)))
                   (and binding (depends-on? (cdr binding) var frame)))))
            ((pair? exp)
             (or (depends-on? (car exp) var frame)
                 (depends-on? (cdr exp) var frame)))
            (else false)))

    ;; Backtracking undoes the increment, but that only reuses the ids of
    ;; renamings made on the abandoned branch, which no frame refers to.
    (define rule-counter 0)

    (define (rename-variables-in rule)
      (set! rule-counter (+ rule-counter 1))
      (rename rule rule-counter))

    (define (rename exp id)
      (cond ((var? exp) (cons '? (cons id (cdr exp))))
            ((pair? exp) (cons (rename (car exp) id) (rename (cdr exp) id)))
            (else exp)))

    (define (instantiate exp frame)
      (cond ((var? exp)
             (let ((binding (binding-in-frame exp frame)))
               (if binding
                   (instantiate (cdr binding) frame)
                   (error "Unknown pat var -- LISP-VALUE" exp))))
            ((pair? exp)
             (cons (instantiate (car exp) frame)
                   (instantiate (cdr exp) frame)))
            (else exp)))))

(define query-environment (apply amb-environment query-program))

(define (run-amb-query query #!optional limit)
  (let ((processed (query-syntax-process query)))
    (map (lambda (frame)
           (instantiate processed
                        frame
                        (lambda (var frame) (contract-question-mark var))))
         (amb-collect `(qeval ',processed '()) query-environment limit))))

(define married-rules
  '((married Minnie Mickey)
    (rule (married ?x ?y)
          (married ?y ?x))))

(define data-base
  (append microshaft-data-base append-to-form-rules married-rules))

(initialize-data-base! data-base)

(check (run-amb-query '(job ?x (computer programmer)))
       => '((job (Hacker Alyssa P) (computer programmer))
            (job (Fect Cy D) (computer programmer))))

(check (run-amb-query '(append-to-form ?x ?y (a b)))
       => '((append-to-form () (a b) (a b))
            (append-to-form (a) (b) (a b))
            (append-to-form (a b) () (a b))))

(for-each
 (lambda (query)
   (check (run-amb-query query) (=> same-elements?) (run-query query)))
 '((lives-near ?x (Bitdiddle Ben))
   (wheel ?who)
   (outranked-by (Reasoner Louis) ?who)
   (and (salary ?person ?amount) (lisp-value > ?amount 50000))
   (and (supervisor ?x ?boss) (not (job ?boss (computer . ?type))))))

(check (run-amb-query '(unknown ?x)) => '())

;; The search is depth first, so an infinite branch hides all the answers
;; after it, where the streams interleave the two disjuncts.
(define disjunction '(or (married Mickey ?x) (supervisor ?x (Bitdiddle Ben))))

(define (value-of-x answer) (caddr (cadr answer)))

(check (map value-of-x (run-amb-query disjunction 4))
       => '(Minnie Minnie Minnie Minnie))
(check (map value-of-x (run-query-head disjunction 4))
       => '(Minnie (Hacker Alyssa P) Minnie (Fect Cy D)))
