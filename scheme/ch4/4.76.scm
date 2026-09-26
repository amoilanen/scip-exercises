(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; Every conjunct is evaluated on its own, starting from the same input
;; frame, and the resulting streams are joined: two output frames combine
;; when their bindings unify.

(define (conjoin conjuncts frame-stream)
  (stream-flatmap (lambda (frame) (conjoin-from-frame conjuncts frame))
                  frame-stream))

(define (conjoin-from-frame conjuncts frame)
  (if (empty-conjunction? conjuncts)
      (singleton-stream frame)
      (join-frame-streams
       (qeval (first-conjunct conjuncts) (singleton-stream frame))
       (conjoin-from-frame (rest-conjuncts conjuncts) frame))))

(define (join-frame-streams frames-1 frames-2)
  (stream-flatmap
   (lambda (frame-1)
     (stream-flatmap
      (lambda (frame-2)
        (let ((merged (merge-frames frame-1 frame-2)))
          (if (eq? merged 'failed)
              the-empty-stream
              (singleton-stream merged))))
      frames-2))
   frames-1))

(define (merge-frames frame-1 frame-2)
  (cond ((eq? frame-2 'failed) 'failed)
        ((null? frame-1) frame-2)
        (else
         (let ((binding (car frame-1)))
           (merge-frames (cdr frame-1)
                         (extend-if-possible (binding-variable binding)
                                             (binding-value binding)
                                             frame-2))))))

(put 'and 'qeval conjoin)

(initialize-data-base! microshaft-data-base)

(check (run-query '(and (supervisor ?person (Bitdiddle Ben))
                        (address ?person ?where)))
       (=> same-elements?)
       '((and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
              (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
         (and (supervisor (Fect Cy D) (Bitdiddle Ben))
              (address (Fect Cy D) (Cambridge (Ames Street) 3)))
         (and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
              (address (Tweakit Lem E) (Boston (Bay State Road) 22)))))

(check (run-query '(and (job ?x (computer programmer))
                        (supervisor ?x ?boss)
                        (job ?boss ?job)))
       (=> same-elements?)
       '((and (job (Hacker Alyssa P) (computer programmer))
              (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
              (job (Bitdiddle Ben) (computer wizard)))
         (and (job (Fect Cy D) (computer programmer))
              (supervisor (Fect Cy D) (Bitdiddle Ben))
              (job (Bitdiddle Ben) (computer wizard)))))

(check (run-query '(wheel ?who))
       (=> same-elements?)
       '((wheel (Warbucks Oliver))
         (wheel (Warbucks Oliver))
         (wheel (Warbucks Oliver))
         (wheel (Warbucks Oliver))
         (wheel (Bitdiddle Ben))))

;; The price is that a conjunct no longer sees the bindings made by the
;; others.  not and lisp-value only filter frames, so they now run with
;; their variables unbound: (not (same ?person-1 ?person-2)) rejects every
;; frame and lives-near finds nobody (exercise 4.77 fixes this).  And a
;; recursive rule whose body is a conjunction, like outranked-by, applies
;; itself with nothing bound and never finishes.
(check (run-query '(lives-near ?x (Bitdiddle Ben))) => '())
(check (run-query-bounded '(outranked-by (Reasoner Louis) ?who) 100)
       => '((outranked-by (Reasoner Louis) (Hacker Alyssa P)) diverged))
