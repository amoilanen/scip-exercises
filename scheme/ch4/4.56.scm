(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/query.scm" (current-load-pathname)))

(initialize-data-base! microshaft-data-base)

(define supervised-by-ben-with-address
  '(and (supervisor ?person (Bitdiddle Ben))
        (address ?person ?where)))

(check (run-query supervised-by-ben-with-address)
       (=> same-elements?)
       '((and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
              (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
         (and (supervisor (Fect Cy D) (Bitdiddle Ben))
              (address (Fect Cy D) (Cambridge (Ames Street) 3)))
         (and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
              (address (Tweakit Lem E) (Boston (Bay State Road) 22)))))

(define earning-less-than-ben
  '(and (salary (Bitdiddle Ben) ?ben-amount)
        (salary ?person ?amount)
        (lisp-value < ?amount ?ben-amount)))

(define (person-in-second-clause answer)
  (cadr (caddr answer)))

(check (map person-in-second-clause (run-query earning-less-than-ben))
       (=> same-elements?)
       '((Hacker Alyssa P) (Fect Cy D) (Tweakit Lem E) (Reasoner Louis)
         (Cratchet Robert) (Aull DeWitt)))

(define supervised-outside-computer-division
  '(and (supervisor ?person ?boss)
        (not (job ?boss (computer . ?position)))
        (job ?boss ?job)))

(check (run-query supervised-outside-computer-division)
       (=> same-elements?)
       '((and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
              (not (job (Warbucks Oliver) (computer . ?position)))
              (job (Warbucks Oliver) (administration big wheel)))
         (and (supervisor (Scrooge Eben) (Warbucks Oliver))
              (not (job (Warbucks Oliver) (computer . ?position)))
              (job (Warbucks Oliver) (administration big wheel)))
         (and (supervisor (Cratchet Robert) (Scrooge Eben))
              (not (job (Scrooge Eben) (computer . ?position)))
              (job (Scrooge Eben) (accounting chief accountant)))
         (and (supervisor (Aull DeWitt) (Warbucks Oliver))
              (not (job (Warbucks Oliver) (computer . ?position)))
              (job (Warbucks Oliver) (administration big wheel)))))
