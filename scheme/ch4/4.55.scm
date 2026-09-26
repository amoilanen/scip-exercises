(load "lib/check.scm")
(load "ch4/lib/query.scm")

(initialize-data-base! microshaft-data-base)

(check (run-query '(supervisor ?x (Bitdiddle Ben)))
       (=> same-elements?)
       '((supervisor (Hacker Alyssa P) (Bitdiddle Ben))
         (supervisor (Fect Cy D) (Bitdiddle Ben))
         (supervisor (Tweakit Lem E) (Bitdiddle Ben))))

(check (run-query '(job ?name (accounting . ?position)))
       (=> same-elements?)
       '((job (Scrooge Eben) (accounting chief accountant))
         (job (Cratchet Robert) (accounting scrivener))))

(check (run-query '(address ?name (Slumerville . ?street)))
       (=> same-elements?)
       '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
         (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
         (address (Aull DeWitt) (Slumerville (Onion Square) 5))))
