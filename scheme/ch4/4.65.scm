(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/query.scm" (current-load-pathname)))

;; wheel produces one frame for every way of satisfying its body, that is
;; for every pair of a middle manager and a person that middle manager
;; supervises.  Oliver Warbucks supervises Ben Bitdiddle, who supervises
;; three people, and Eben Scrooge, who supervises one, so he appears four
;; times.  (DeWitt Aull supervises nobody and contributes nothing.)

(initialize-data-base! microshaft-data-base)

(check (run-query '(wheel ?who))
       (=> same-elements?)
       '((wheel (Warbucks Oliver))
         (wheel (Warbucks Oliver))
         (wheel (Warbucks Oliver))
         (wheel (Warbucks Oliver))
         (wheel (Bitdiddle Ben))))

(check (run-query '(and (supervisor ?middle-manager (Warbucks Oliver))
                        (supervisor ?x ?middle-manager)))
       (=> same-elements?)
       '((and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
              (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
         (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
              (supervisor (Fect Cy D) (Bitdiddle Ben)))
         (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
              (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
         (and (supervisor (Scrooge Eben) (Warbucks Oliver))
              (supervisor (Cratchet Robert) (Scrooge Eben)))))
