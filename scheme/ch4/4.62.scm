(load "lib/check.scm")
(load "ch4/lib/query.scm")

(initialize-data-base!
 '((rule (last-pair (?x) (?x)))
   (rule (last-pair (?head . ?tail) ?x)
         (last-pair ?tail ?x))))

(check (run-query '(last-pair (3) ?x)) => '((last-pair (3) (3))))
(check (run-query '(last-pair (1 2 3) ?x)) => '((last-pair (1 2 3) (3))))
(check (run-query '(last-pair (2 ?x) (3))) => '((last-pair (2 3) (3))))
(check (run-query '(last-pair () ?x)) => '())

;; (last-pair ?x (3)) has infinitely many answers: every list ending in 3.
;; The rules do not loop, but the answer stream never ends.
(define (list-and-last answer)
  (let ((list (cadr answer)))
    (cons (length list) (last list))))

(check (map list-and-last (run-query-head '(last-pair ?x (3)) 4))
       => '((1 . 3) (2 . 3) (3 . 3) (4 . 3)))
