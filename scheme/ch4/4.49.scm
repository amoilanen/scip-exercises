(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/amb.scm" (current-load-pathname)))

;; To generate sentences, parse-word ignores the input and chooses any word
;; of the required category.  The search is depth first and always revises
;; the most recent choice, so after the first sentence it keeps extending
;; the last phrase with more prepositional phrases and never gets to try
;; another word for the earlier choices.

(define generator-program
  (append parser-program
          '((define (parse-word word-list)
              (list (car word-list) (an-element-of (cdr word-list)))))))

(define (sentence-words tree)
  (if (symbol? (cadr tree))
      (list (cadr tree))
      (append-map sentence-words (cdr tree))))

(define env (apply amb-environment generator-program))

(check (map sentence-words (amb-collect '(parse-sentence) env 4))
       => '((the student studies)
            (the student studies for the student)
            (the student studies for the student for the student)
            (the student studies for the student for the student
                 for the student)))

;; Without the recursive phrases every combination of 2 articles, 4 nouns
;; and 4 verbs is generated.
(let ((simple-sentences
       (map sentence-words
            (amb-collect '(list 'sentence
                                (parse-simple-noun-phrase)
                                (parse-word verbs))
                         env))))
  (check (length simple-sentences) => 32)
  (check (length (delete-duplicates simple-sentences)) => 32)
  (check (list-head simple-sentences 5)
         => '((the student studies) (the student lectures)
              (the student eats) (the student sleeps)
              (the professor studies))))
