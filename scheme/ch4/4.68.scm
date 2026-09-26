(load "lib/check.scm")
(load "ch4/lib/query.scm")

(initialize-data-base!
 (append append-to-form-rules
         '((rule (reverse () ()))
           (rule (reverse (?x . ?rest) ?y)
                 (and (reverse ?rest ?reversed-rest)
                      (append-to-form ?reversed-rest (?x) ?y))))))

(check (run-query '(reverse (1 2 3) ?x)) => '((reverse (1 2 3) (3 2 1))))
(check (run-query '(reverse () ?x)) => '((reverse () ())))

;; These rules cannot answer (reverse ?x (1 2 3)).  With ?x unknown,
;; (reverse ?rest ?reversed-rest) has nothing bound and enumerates lists of
;; every length.  The answer is found, but the search never ends.
(check (run-query-bounded '(reverse ?x (1 2 3)) 100)
       => '((reverse (3 2 1) (1 2 3)) diverged))

;; Fixing the length of both lists first makes the rules work both ways:
;; same-length terminates as soon as either list is known, after which
;; reversing onto an accumulator walks a list of known length.
(initialize-data-base!
 '((rule (same-length () ()))
   (rule (same-length (?x . ?xs) (?y . ?ys))
         (same-length ?xs ?ys))

   (rule (reverse-onto () ?reversed ?reversed))
   (rule (reverse-onto (?x . ?rest) ?accumulated ?reversed)
         (reverse-onto ?rest (?x . ?accumulated) ?reversed))

   (rule (reverse ?list ?reversed)
         (and (same-length ?list ?reversed)
              (reverse-onto ?list () ?reversed)))))

(check (run-query '(reverse (1 2 3) ?x)) => '((reverse (1 2 3) (3 2 1))))
(check (run-query '(reverse ?x (1 2 3))) => '((reverse (3 2 1) (1 2 3))))
(check (run-query '(reverse (1 ?x) (2 ?y))) => '((reverse (1 2) (2 1))))
(check (run-query '(reverse (1 2) (1 2))) => '())
(check (run-query '(reverse () ?x)) => '((reverse () ())))
