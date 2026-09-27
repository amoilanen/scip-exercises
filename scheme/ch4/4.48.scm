(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/amb.scm" (current-load-pathname)))

;; Adjectives may precede a noun, an adverb may follow a verb, and sentences
;; may be joined by conjunctions.

(define extended-grammar
  (append
   parser-program
   '((define adjectives '(adjective big lazy clever old))
     (define adverbs '(adverb quickly slowly often))
     (define conjunctions '(conjunction and but))

     (define (parse-sentence)
       (let ((simple (parse-simple-sentence)))
         (amb simple
              (list 'compound-sentence
                    simple
                    (parse-word conjunctions)
                    (parse-sentence)))))

     (define (parse-simple-sentence)
       (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

     (define (parse-simple-noun-phrase)
       (list 'simple-noun-phrase
             (parse-word articles)
             (parse-described-noun)))

     (define (parse-described-noun)
       (amb (parse-word nouns)
            (list 'described-noun
                  (parse-word adjectives)
                  (parse-described-noun))))

     (define (parse-verb-phrase)
       (define (maybe-extend verb-phrase)
         (amb verb-phrase
              (maybe-extend (list 'verb-phrase
                                  verb-phrase
                                  (parse-prepositional-phrase)))))
       (maybe-extend (parse-modified-verb)))

     (define (parse-modified-verb)
       (let ((verb (parse-word verbs)))
         (amb verb
              (list 'modified-verb verb (parse-word adverbs))))))))

(define env (apply amb-environment extended-grammar))

(check (amb-collect '(parse '(the big lazy cat sleeps)) env)
       => '((sentence
             (simple-noun-phrase
              (article the)
              (described-noun (adjective big)
                              (described-noun (adjective lazy) (noun cat))))
             (verb sleeps))))

(check (amb-collect '(parse '(a professor lectures slowly in the class)) env)
       => '((sentence
             (simple-noun-phrase (article a) (noun professor))
             (verb-phrase
              (modified-verb (verb lectures) (adverb slowly))
              (prep-phrase (prep in)
                           (simple-noun-phrase (article the)
                                               (noun class)))))))

(check (amb-collect '(parse '(the cat eats but the old student studies))
                    env)
       => '((compound-sentence
             (sentence (simple-noun-phrase (article the) (noun cat))
                       (verb eats))
             (conjunction but)
             (sentence (simple-noun-phrase
                        (article the)
                        (described-noun (adjective old) (noun student)))
                       (verb studies)))))

(check (length (amb-collect
                '(parse '(the cat sleeps and the cat eats and a cat studies))
                env))
       => 1)

(check (amb-collect '(parse '(the cat slowly)) env) => '())
(check (amb-collect '(parse '(the cat sleeps and)) env) => '())
(check (amb-collect '(parse '(big cat sleeps)) env) => '())
