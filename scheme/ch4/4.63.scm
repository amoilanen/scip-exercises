(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/query.scm" (current-load-pathname)))

(initialize-data-base!
 (append genesis-data-base
         '((rule (son ?man ?son)
                 (and (wife ?man ?woman)
                      (son ?woman ?son)))
           (rule (grandson ?grandfather ?grandson)
                 (and (son ?grandfather ?father)
                      (son ?father ?grandson))))))

(check (run-query '(grandson Cain ?s)) => '((grandson Cain Irad)))

(check (run-query '(son Lamech ?s))
       (=> same-elements?)
       '((son Lamech Jabal) (son Lamech Jubal)))

(check (run-query '(grandson Methushael ?s))
       (=> same-elements?)
       '((grandson Methushael Jabal) (grandson Methushael Jubal)))

(check (run-query '(grandson ?g Enoch)) => '((grandson Adam Enoch)))
