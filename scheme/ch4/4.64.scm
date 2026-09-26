(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; After the first disjunct has found Ben's supervisor, the second one starts
;; with (outranked-by ?middle-manager ?boss), in which nothing is bound:
;; Ben only enters through the supervisor clause that comes after it.  That
;; application of the rule again reaches (outranked-by ?middle-manager ?boss)
;; with fresh unbound variables, and so on without end.  Every level adds
;; more frames for the pending supervisor filter to reject, so the system
;; prints the one answer and then runs forever.

(define louis-outranked-by
  '(rule (outranked-by ?staff-person ?boss)
         (or (supervisor ?staff-person ?boss)
             (and (outranked-by ?middle-manager ?boss)
                  (supervisor ?staff-person ?middle-manager)))))

(define query '(outranked-by (Bitdiddle Ben) ?who))

(initialize-data-base! (cons louis-outranked-by microshaft-assertions))

(check (run-query-bounded query 50)
       => '((outranked-by (Bitdiddle Ben) (Warbucks Oliver)) diverged))

(initialize-data-base! microshaft-data-base)

(check (run-query query)
       => '((outranked-by (Bitdiddle Ben) (Warbucks Oliver))))
