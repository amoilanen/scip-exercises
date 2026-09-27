(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/query.scm" (current-load-pathname)))

(initialize-data-base! microshaft-data-base)

(define programmer-free-subordinates
  '(and (not (job ?x (computer programmer)))
        (supervisor ?x (Bitdiddle Ben))))

(define well-paid
  '(and (lisp-value > ?amount 50000)
        (salary ?person ?amount)))

(check (run-query programmer-free-subordinates) => '())
(check-error (run-query well-paid))

;; A filter whose pattern still has unbound variables is not applied yet but
;; stored in the frame, as a "promise" under the non-variable key
;; delayed-filters.  Every extension of a frame applies the stored filters
;; that have become fully bound, failing the frame if one of them rejects
;; it.  Filters that never become fully bound, because their pattern has
;; variables of its own like ?type in (not (job ?x (computer . ?type))), are
;; applied as before once the query, or the query of a not, has produced
;; its frames.

(define (make-filter pattern passes?) (cons pattern passes?))
(define (filter-pattern filter) (car filter))
(define (filter-passes? filter frame) ((cdr filter) frame))

(define (delayed-filters frame)
  (let ((binding (binding-in-frame 'delayed-filters frame)))
    (if binding (binding-value binding) '())))

(define (with-delayed-filters filters frame)
  (cons (make-binding 'delayed-filters filters) frame))

(define (fully-bound? pattern frame)
  (let walk ((exp (instantiate pattern frame (lambda (var frame) var))))
    (cond ((var? exp) #f)
          ((pair? exp) (and (walk (car exp)) (walk (cdr exp))))
          (else #t))))

(define (apply-filters filters frame)
  (if (every (lambda (filter) (filter-passes? filter frame)) filters)
      frame
      'failed))

(define (apply-ready-filters frame)
  (let ((filters (delayed-filters frame)))
    (receive (ready waiting)
        (partition (lambda (filter)
                     (fully-bound? (filter-pattern filter) frame))
                   filters)
      (if (null? ready)
          frame
          (let ((result (apply-filters
                         ready
                         (with-delayed-filters '() frame))))
            (if (eq? result 'failed)
                'failed
                (with-delayed-filters waiting frame)))))))

(define (extend variable value frame)
  (apply-ready-filters (cons (make-binding variable value) frame)))

(define (apply-remaining-filters frame-stream)
  (stream-filter
   (lambda (frame) (not (eq? frame 'failed)))
   (stream-map (lambda (frame)
                 (apply-filters (delayed-filters frame)
                                (with-delayed-filters '() frame)))
               frame-stream)))

(define (query-frames query)
  (apply-remaining-filters (qeval query (singleton-stream empty-frame))))

(define (filter-frame-stream pattern passes? frame-stream)
  (stream-flatmap
   (lambda (frame)
     (cond ((not (fully-bound? pattern frame))
            (singleton-stream
             (with-delayed-filters
              (cons (make-filter pattern passes?) (delayed-filters frame))
              frame)))
           ((passes? frame) (singleton-stream frame))
           (else the-empty-stream)))
   frame-stream))

(define (negate operands frame-stream)
  (let ((query (negated-query operands)))
    (filter-frame-stream
     query
     (lambda (frame)
       (stream-null?
        (apply-remaining-filters (qeval query (singleton-stream frame)))))
     frame-stream)))

(define (lisp-value call frame-stream)
  (filter-frame-stream
   call
   (lambda (frame)
     (execute (instantiate call
                           frame
                           (lambda (var frame)
                             (error "Unknown pat var -- LISP-VALUE" var)))))
   frame-stream))

(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)

(check (run-query programmer-free-subordinates)
       => '((and (not (job (Tweakit Lem E) (computer programmer)))
                 (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))

(check (run-query '(and (not (job ?x (computer programmer)))
                        (same ?x ?y)
                        (supervisor ?y (Bitdiddle Ben))))
       => '((and (not (job (Tweakit Lem E) (computer programmer)))
                 (same (Tweakit Lem E) (Tweakit Lem E))
                 (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))

(check (map caddr (run-query well-paid))
       (=> same-elements?)
       '((salary (Bitdiddle Ben) 60000)
         (salary (Warbucks Oliver) 150000)
         (salary (Scrooge Eben) 75000)))

(check (run-query '(and (not (not (job ?x (computer wizard))))
                        (job ?x ?job)))
       => '((and (not (not (job (Bitdiddle Ben) (computer wizard))))
                 (job (Bitdiddle Ben) (computer wizard)))))

(check (run-query '(and (supervisor ?x ?boss)
                        (not (job ?boss (computer . ?type)))
                        (job ?boss (accounting . ?position))))
       => '((and (supervisor (Cratchet Robert) (Scrooge Eben))
                 (not (job (Scrooge Eben) (computer . ?type)))
                 (job (Scrooge Eben) (accounting chief accountant)))))

(add-to-data-base!
 '((rule (lives-near-filter-first ?person-1 ?person-2)
         (and (not (same ?person-1 ?person-2))
              (address ?person-1 (?town . ?rest-1))
              (address ?person-2 (?town . ?rest-2))))))

(check (run-query '(lives-near-filter-first ?x (Bitdiddle Ben)))
       (=> same-elements?)
       '((lives-near-filter-first (Reasoner Louis) (Bitdiddle Ben))
         (lives-near-filter-first (Aull DeWitt) (Bitdiddle Ben))))

(check (run-query '(lives-near ?x (Hacker Alyssa P)))
       => '((lives-near (Fect Cy D) (Hacker Alyssa P))))
