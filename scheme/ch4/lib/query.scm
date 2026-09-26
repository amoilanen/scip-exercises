;; The query system of section 4.4.4, run without a driver loop.
;;
;;   (initialize-data-base! items)   empties the data base and adds ITEMS,
;;                                   assertions and (rule ...) forms alike
;;   (add-to-data-base! items)       adds ITEMS to the current data base,
;;                                   ahead of the older items; within ITEMS
;;                                   the order is kept, so rules are tried
;;                                   in the order they are listed
;;   (run-query query)               list of the instantiated answers
;;   (run-query-head query n)        at most the first N answers
;;   (run-query-bounded query steps) the answers found before qeval has been
;;                                   called STEPS times, followed by the
;;                                   symbol diverged if the limit was hit
;;
;; Ready to be installed: microshaft-data-base, the assertions and rules of
;; section 4.4.1 (also available separately as microshaft-assertions and
;; microshaft-rules), append-to-form-rules and genesis-data-base (exercise
;; 4.63).  same-elements? compares answer lists as multisets.

;;; Streams

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (interleave-delayed (force delayed-s2)
                                       (delay (stream-cdr s1))))))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed (stream-car stream)
                          (delay (flatten-stream (stream-cdr stream))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

;;; Operation table used to dispatch on special forms

(define operation-table (make-equal-hash-table))

(define (put op type item)
  (hash-table-set! operation-table (cons op type) item))

(define (get op type)
  (hash-table-ref/default operation-table (cons op type) #f))

;;; Driver

(define (query-frames query)
  (qeval query (singleton-stream empty-frame)))

(define (query-results query)
  (let ((processed (query-syntax-process query)))
    (stream-map (lambda (frame)
                  (instantiate processed
                               frame
                               (lambda (var frame)
                                 (contract-question-mark var))))
                (query-frames processed))))

(define (run-query query)
  (stream->list (query-results query)))

(define (run-query-head query n)
  (let loop ((results (query-results query)) (n n))
    (cond ((or (= n 0) (stream-null? results)) '())
          ((= n 1) (list (stream-car results)))
          (else (cons (stream-car results)
                      (loop (stream-cdr results) (- n 1)))))))

(define (run-query-bounded query step-limit)
  (let* ((found '())
         (plain-qeval qeval)
         (outcome
          (call-with-current-continuation
           (lambda (give-up)
             (define (counting-qeval query frame-stream)
               (set! step-limit (- step-limit 1))
               (if (< step-limit 0)
                   (give-up 'diverged))
               (plain-qeval query frame-stream))
             (dynamic-wind
              (lambda () (set! qeval counting-qeval))
              (lambda ()
                (stream-for-each (lambda (answer)
                                   (set! found (cons answer found)))
                                 (query-results query))
                'finished)
              (lambda () (set! qeval plain-qeval)))))))
    (if (eq? outcome 'diverged)
        (reverse (cons 'diverged found))
        (reverse found))))

(define (same-elements? xs ys)
  (define (remove-one x ys)
    (cond ((null? ys) #f)
          ((equal? x (car ys)) (cdr ys))
          (else (let ((rest (remove-one x (cdr ys))))
                  (and rest (cons (car ys) rest))))))
  (cond ((null? xs) (null? ys))
        ((remove-one (car xs) ys)
         => (lambda (rest) (same-elements? (cdr xs) rest)))
        (else #f)))

;;; The evaluator

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed (find-assertions query-pattern frame)
                            (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute (instantiate call
                               frame
                               (lambda (var frame)
                                 (error "Unknown pat var -- LISP-VALUE"
                                        var))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'and 'qeval conjoin)
(put 'or 'qeval disjoin)
(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)
(put 'always-true 'qeval always-true)

;;; Assertions and pattern matching

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum) (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pattern query-frame)
  (let ((match-result (pattern-match query-pattern assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

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
        (pattern-match (binding-value binding) datum frame)
        (extend var datum frame))))

;;; Rules and unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule) (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let* ((clean-rule (rename-variables-in rule))
         (unify-result
          (unify-match query-pattern (conclusion clean-rule) query-frame)))
    (if (eq? unify-result 'failed)
        the-empty-stream
        (qeval (rule-body clean-rule) (singleton-stream unify-result)))))

(define (rename-variables-in rule)
  (let ((id (new-rule-application-id)))
    (let walk ((exp rule))
      (cond ((var? exp) (make-new-variable exp id))
            ((pair? exp) (cons (walk (car exp)) (walk (cdr exp))))
            (else exp)))))

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

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((and (var? val) (binding-in-frame val frame))
           => (lambda (val-binding)
                (unify-match var (binding-value val-binding) frame)))
          ((depends-on? val var frame) 'failed)
          (else (extend var val frame)))))

;; True if EXP refers to VAR, directly or through the bindings in FRAME:
;; binding VAR to EXP would make VAR part of its own value.
(define (depends-on? exp var frame)
  (let walk ((e exp))
    (cond ((var? e)
           (or (equal? var e)
               (let ((binding (binding-in-frame e frame)))
                 (and binding (walk (binding-value binding))))))
          ((pair? e) (or (walk (car e)) (walk (cdr e))))
          (else #f))))

;;; The data base

(define the-assertions the-empty-stream)
(define the-rules the-empty-stream)
(define assertion-index (make-strong-eqv-hash-table))
(define rule-index (make-strong-eqv-hash-table))

(define (reset-data-base!)
  (set! the-assertions the-empty-stream)
  (set! the-rules the-empty-stream)
  (hash-table-clear! assertion-index)
  (hash-table-clear! rule-index))

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-stream assertion-index (index-key-of pattern))
      the-assertions))

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (stream-append-delayed (get-stream rule-index (index-key-of pattern))
                             (delay (get-stream rule-index '?)))
      the-rules))

(define (get-stream index key)
  (hash-table-ref/default index key the-empty-stream))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-in-index! assertion-index assertion assertion)
  (let ((old-assertions the-assertions))
    (set! the-assertions (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-in-index! rule-index (conclusion rule) rule)
  (let ((old-rules the-rules))
    (set! the-rules (cons-stream rule old-rules))
    'ok))

;; Items are indexed by the symbol in the car of PATTERN; patterns that
;; start with a variable go under the key ?.
(define (store-in-index! index pattern item)
  (if (indexable? pattern)
      (let* ((key (index-key-of pattern))
             (current (get-stream index key)))
        (hash-table-set! index key (cons-stream item current)))))

(define (indexable? pattern)
  (or (constant-symbol? (car pattern))
      (var? (car pattern))))

(define (index-key-of pattern)
  (let ((key (car pattern)))
    (if (var? key) '? key)))

(define (use-index? pattern)
  (constant-symbol? (car pattern)))

(define (add-to-data-base! items)
  (for-each (lambda (item)
              (add-rule-or-assertion! (query-syntax-process item)))
            (reverse items)))

(define (initialize-data-base! items)
  (reset-data-base!)
  (add-to-data-base! items))

;;; Query syntax

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define empty-conjunction? null?)
(define first-conjunct car)
(define rest-conjuncts cdr)
(define empty-disjunction? null?)
(define first-disjunct car)
(define rest-disjuncts cdr)
(define negated-query car)
(define predicate car)
(define args cdr)

(define (rule? statement)
  (and (pair? statement) (eq? (car statement) 'rule)))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

;; ?x is read as (? x); a variable renamed by a rule application
;; becomes (? id x).
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string-prefix? "?" chars)
        (list '? (string->symbol (string-tail chars 1)))
        symbol)))

(define (var? exp)
  (and (pair? exp) (eq? (car exp) '?)))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ rule-counter 1))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

(define (instantiate exp frame unbound-var-handler)
  (let copy ((exp exp))
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp) (cons (copy (car exp)) (copy (cdr exp))))
          (else exp))))

;;; Frames

(define empty-frame '())

(define (make-binding variable value) (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;;; Data bases

(define microshaft-assertions
  '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    (can-do-job (computer programmer) (computer programmer trainee))
    (can-do-job (administration secretary) (administration big wheel))))

(define microshaft-rules
  '((rule (same ?x ?x))

    (rule (lives-near ?person-1 ?person-2)
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (wheel ?person)
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))))

(define microshaft-data-base
  (append microshaft-assertions microshaft-rules))

(define append-to-form-rules
  '((rule (append-to-form () ?y ?y))
    (rule (append-to-form (?u . ?v) ?y (?u . ?z))
          (append-to-form ?v ?y ?z))))

(define genesis-data-base
  '((son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)))
