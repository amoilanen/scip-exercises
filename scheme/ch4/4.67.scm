(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; Each frame carries the goals whose rule applications it is currently
;; inside of, stored under the non-variable key history.  A goal is the
;; query pattern instantiated in the frame, with its unbound variables
;; renamed canonically so that goals equal up to renaming compare equal.
;; A rule is not applied to a goal already in the history: that derivation
;; would only repeat itself.  The frames leaving a rule body get the history
;; they came in with back, so later conjuncts don't inherit stale goals.

(define (canonical-goal pattern frame)
  (let ((renamings '()))
    (instantiate pattern
                 frame
                 (lambda (var frame)
                   (let ((renaming (assoc var renamings)))
                     (if renaming
                         (cdr renaming)
                         (let ((name (list '? (length renamings))))
                           (set! renamings (cons (cons var name) renamings))
                           name)))))))

(define (goal-history frame)
  (let ((binding (binding-in-frame 'history frame)))
    (if binding (binding-value binding) '())))

(define (with-goal-history history frame)
  (extend 'history history frame))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((goal (canonical-goal query-pattern query-frame))
        (history (goal-history query-frame)))
    (if (member goal history)
        the-empty-stream
        (let* ((clean-rule (rename-variables-in rule))
               (unify-result
                (unify-match query-pattern
                             (conclusion clean-rule)
                             (with-goal-history (cons goal history)
                                                query-frame))))
          (if (eq? unify-result 'failed)
              the-empty-stream
              (stream-map (lambda (frame)
                            (with-goal-history history frame))
                          (qeval (rule-body clean-rule)
                                 (singleton-stream unify-result))))))))

(initialize-data-base!
 '((married Minnie Mickey)
   (rule (married ?x ?y)
         (married ?y ?x))))

(check (run-query '(married Mickey ?who)) => '((married Mickey Minnie)))

(check (run-query '(married ?x ?y))
       (=> same-elements?)
       '((married Minnie Mickey) (married Mickey Minnie)))

(check (run-query '(and (married Mickey ?a) (married Mickey ?b)))
       => '((and (married Mickey Minnie) (married Mickey Minnie))))

(define louis-outranked-by
  '(rule (outranked-by ?staff-person ?boss)
         (or (supervisor ?staff-person ?boss)
             (and (outranked-by ?middle-manager ?boss)
                  (supervisor ?staff-person ?middle-manager)))))

(initialize-data-base! (cons louis-outranked-by microshaft-assertions))

(check (run-query '(outranked-by (Bitdiddle Ben) ?who))
       => '((outranked-by (Bitdiddle Ben) (Warbucks Oliver))))

;; The detector stops simple loops but is not complete: a goal met again may
;; be needed once more to reach a new answer.  With Louis's rule, Oliver is
;; two levels above Louis Reasoner, and deriving that means solving
;; (outranked-by ?m ?boss) inside a derivation of that very goal, so he is
;; no longer found.
(check (run-query '(outranked-by (Reasoner Louis) ?who))
       (=> same-elements?)
       '((outranked-by (Reasoner Louis) (Hacker Alyssa P))
         (outranked-by (Reasoner Louis) (Bitdiddle Ben))))

(initialize-data-base! (append microshaft-data-base append-to-form-rules))

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
