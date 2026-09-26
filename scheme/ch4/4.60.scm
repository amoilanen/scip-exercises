(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; lives-near is symmetric: whenever the frame with ?person-1 = A and
;; ?person-2 = B satisfies the rule, so does the one with the roles swapped,
;; and the query system has no way to know that the two answers describe the
;; same pair.  Requiring the two names to be in a fixed order keeps exactly
;; one of the two frames.

(define (name<? name-1 name-2)
  (string<? (write-to-string name-1) (write-to-string name-2)))

(initialize-data-base!
 (append microshaft-data-base
         '((rule (lives-near-pair ?person-1 ?person-2)
                 (and (lives-near ?person-1 ?person-2)
                      (lisp-value name<? ?person-1 ?person-2))))))

(check (run-query '(lives-near ?person (Hacker Alyssa P)))
       => '((lives-near (Fect Cy D) (Hacker Alyssa P))))

(check (run-query '(lives-near (Hacker Alyssa P) ?person))
       => '((lives-near (Hacker Alyssa P) (Fect Cy D))))

(check (length (run-query '(lives-near ?person-1 ?person-2))) => 8)

(check (run-query '(lives-near-pair ?person-1 ?person-2))
       (=> same-elements?)
       '((lives-near-pair (Aull DeWitt) (Bitdiddle Ben))
         (lives-near-pair (Aull DeWitt) (Reasoner Louis))
         (lives-near-pair (Bitdiddle Ben) (Reasoner Louis))
         (lives-near-pair (Fect Cy D) (Hacker Alyssa P))))
