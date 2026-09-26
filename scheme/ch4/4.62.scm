(load "lib/check.scm")
(load "ch4/lib/query.scm")

(define last-pair-rules
  '((rule (last-pair (?x) (?x)))
    (rule (last-pair (?head . ?tail) ?x)
          (last-pair ?tail ?x))))

(initialize-data-base! last-pair-rules)

(check (run-query '(last-pair (3) ?x)) => '((last-pair (3) (3))))
(check (run-query '(last-pair (1 2 3) ?x)) => '((last-pair (1 2 3) (3))))
(check (run-query '(last-pair (2 ?x) (3))) => '((last-pair (2 3) (3))))
(check (run-query '(last-pair () ?x)) => '())

;; (last-pair ?x (3)) has infinitely many answers, one for every list that
;; ends in 3, so the answer stream never ends.
(define (length-and-last answer)
  (let ((list (cadr answer)))
    (cons (length list) (last list))))

(check (map length-and-last (run-query-head '(last-pair ?x (3)) 4))
       => '((1 . 3) (2 . 3) (3 . 3) (4 . 3)))

;; With the recursive rule tried first, the system keeps applying it to
;; ever longer unknown lists and never produces even the first answer.
(initialize-data-base! (reverse last-pair-rules))

(check (run-query '(last-pair (1 2 3) ?x)) => '((last-pair (1 2 3) (3))))
(check (run-query-bounded '(last-pair ?x (3)) 1000) => '(diverged))
