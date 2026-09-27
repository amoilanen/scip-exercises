(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/query.scm" (current-load-pathname)))

(initialize-data-base!
 '((rule (?x next-to ?y in (?x ?y . ?u)))
   (rule (?x next-to ?y in (?v . ?z))
         (?x next-to ?y in ?z))))

(check (run-query '(?x next-to ?y in (1 (2 3) 4)))
       (=> same-elements?)
       '((1 next-to (2 3) in (1 (2 3) 4))
         ((2 3) next-to 4 in (1 (2 3) 4))))

(check (run-query '(?x next-to 1 in (2 1 3 1)))
       (=> same-elements?)
       '((2 next-to 1 in (2 1 3 1))
         (3 next-to 1 in (2 1 3 1))))

(check (run-query '(?x next-to ?y in (1))) => '())
