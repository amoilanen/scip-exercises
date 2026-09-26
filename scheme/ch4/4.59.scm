(load "lib/check.scm")
(load "ch4/lib/query.scm")

(initialize-data-base!
 (append microshaft-data-base
         '((meeting accounting (Monday 9am))
           (meeting administration (Monday 10am))
           (meeting computer (Wednesday 3pm))
           (meeting administration (Friday 1pm))
           (meeting whole-company (Wednesday 4pm))

           (rule (meeting-time ?person ?day-and-time)
                 (or (meeting whole-company ?day-and-time)
                     (and (job ?person (?division . ?position))
                          (meeting ?division ?day-and-time)))))))

(check (run-query '(meeting ?division (Friday ?time)))
       => '((meeting administration (Friday 1pm))))

(check (run-query '(meeting-time (Hacker Alyssa P) (Wednesday ?time)))
       (=> same-elements?)
       '((meeting-time (Hacker Alyssa P) (Wednesday 3pm))
         (meeting-time (Hacker Alyssa P) (Wednesday 4pm))))

(check (run-query '(meeting-time (Scrooge Eben) (Monday ?time)))
       => '((meeting-time (Scrooge Eben) (Monday 9am))))
