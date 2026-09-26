(load "lib/check.scm")
(load "ch4/lib/query.scm")

(initialize-data-base!
 (append genesis-data-base
         '((rule (son ?man ?son)
                 (and (wife ?man ?woman)
                      (son ?woman ?son)))
           (rule (grandson ?grandfather ?grandson)
                 (and (son ?grandfather ?father)
                      (son ?father ?grandson)))

           (rule (ends-in-grandson (grandson)))
           (rule (ends-in-grandson (?x . ?rest))
                 (ends-in-grandson ?rest))

           (rule ((grandson) ?x ?y)
                 (grandson ?x ?y))
           (rule ((great . ?relationship) ?x ?y)
                 (and (ends-in-grandson ?relationship)
                      (son ?x ?son)
                      (?relationship ?son ?y))))))

(check (run-query '((great grandson) ?g ?ggs))
       (=> same-elements?)
       '(((great grandson) Adam Irad)
         ((great grandson) Cain Mehujael)
         ((great grandson) Enoch Methushael)
         ((great grandson) Irad Lamech)
         ((great grandson) Mehujael Jabal)
         ((great grandson) Mehujael Jubal)))

(check (run-query '((great great great grandson) Adam ?who))
       => '(((great great great grandson) Adam Methushael)))

;; ends-in-grandson generates ever longer relationships, so the one answer
;; is found and the search then continues forever.
(check (run-query-head '(?relationship Adam Irad) 1)
       => '(((great grandson) Adam Irad)))
(check (run-query-bounded '(?relationship Adam Irad) 300)
       => '(((great grandson) Adam Irad) diverged))
