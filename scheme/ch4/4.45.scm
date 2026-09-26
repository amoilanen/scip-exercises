(load "lib/check.scm")
(load "ch4/lib/amb.scm")

;; Reduces a parse to how its phrases attach: a simple noun phrase becomes
;; its noun, a prepositional phrase (preposition object), and a phrase with
;; a modifier (phrase modifier).
(define (attachments tree)
  (case (car tree)
    ((sentence verb-phrase noun-phrase) (map attachments (cdr tree)))
    ((prep-phrase) (list (cadr (cadr tree)) (attachments (caddr tree))))
    ((simple-noun-phrase) (cadr (caddr tree)))
    ((verb) (cadr tree))
    (else (error "Unknown phrase -- ATTACHMENTS" tree))))

(define env (apply amb-environment parser-program))

(define parses
  (amb-collect
   '(parse '(the professor lectures to the student in the class with the cat))
   env))

;; In the order the parser finds them:
;; 1. The professor, having the cat, lectures to the student, in the class.
;; 2. The professor lectures to the student in the class that has the cat.
;; 3. The professor, having the cat, lectures to the student who is in the
;;    class.
;; 4. The professor lectures to the student who is in the class and has the
;;    cat.
;; 5. The professor lectures to the student who is in the class that has
;;    the cat.
(check (map attachments parses)
       => '((professor (((lectures (to student)) (in class)) (with cat)))
            (professor ((lectures (to student)) (in (class (with cat)))))
            (professor ((lectures (to (student (in class)))) (with cat)))
            (professor (lectures (to ((student (in class)) (with cat)))))
            (professor (lectures (to (student (in (class (with cat)))))))))

(check (car parses)
       => '(sentence
            (simple-noun-phrase (article the) (noun professor))
            (verb-phrase
             (verb-phrase
              (verb-phrase
               (verb lectures)
               (prep-phrase (prep to)
                            (simple-noun-phrase (article the)
                                                (noun student))))
              (prep-phrase (prep in)
                           (simple-noun-phrase (article the) (noun class))))
             (prep-phrase (prep with)
                          (simple-noun-phrase (article the) (noun cat))))))
