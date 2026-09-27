(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/query.scm" (current-load-pathname)))

(initialize-data-base!
 (append microshaft-data-base
         '((rule (big-shot ?person ?division)
                 (and (job ?person (?division . ?position))
                      (not (and (supervisor ?person ?boss)
                                (job ?boss (?division . ?boss-position)))))))))

(check (run-query '(big-shot ?who ?division))
       (=> same-elements?)
       '((big-shot (Warbucks Oliver) administration)
         (big-shot (Bitdiddle Ben) computer)
         (big-shot (Scrooge Eben) accounting)))

(check (run-query '(big-shot (Hacker Alyssa P) ?division)) => '())
