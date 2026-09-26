(load "lib/check.scm")
(load "ch4/lib/amb.scm")

;; Louis's version finds the first parse, but asking for another one sends
;; it into an infinite loop: the second alternative calls parse-verb-phrase
;; again before consuming any input, so after every failure the same verb
;; phrase is parsed again, one level deeper, forever.  Interchanging the
;; expressions in the amb makes it worse: parse-verb-phrase then recurs
;; without consuming input before it tries anything else, so it loops even
;; while looking for the first parse.

(define louis-program
  (append parser-program
          '((define (parse-verb-phrase)
              (amb (parse-word verbs)
                   (list 'verb-phrase
                         (parse-verb-phrase)
                         (parse-prepositional-phrase)))))))

(define swapped-program
  (append parser-program
          '((define (parse-verb-phrase)
              (amb (list 'verb-phrase
                         (parse-verb-phrase)
                         (parse-prepositional-phrase))
                   (parse-word verbs))))))

(define sentence '(parse '(the professor lectures to the student)))

(define (all-parses program)
  (with-application-budget
   5000
   (lambda () (amb-collect sentence (apply amb-environment program)))))

(define (first-parse program)
  (with-application-budget
   5000
   (lambda () (amb-collect sentence (apply amb-environment program) 1))))

(check (all-parses parser-program)
       => '((sentence
             (simple-noun-phrase (article the) (noun professor))
             (verb-phrase
              (verb lectures)
              (prep-phrase (prep to)
                           (simple-noun-phrase (article the)
                                               (noun student)))))))

(check (first-parse louis-program) => (all-parses parser-program))
(check (all-parses louis-program) => 'out-of-budget)

(check (first-parse swapped-program) => 'out-of-budget)
