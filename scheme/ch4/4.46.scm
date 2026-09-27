(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/amb.scm" (current-load-pathname)))

;; The parser consumes the input through the shared variable *unparsed*, so
;; the parts of a phrase must be parsed in the order in which they occur in
;; the sentence.  In (list 'sentence (parse-noun-phrase) (parse-verb-phrase))
;; that order is the order in which the operands are evaluated.  Evaluating
;; them right to left would look for the verb phrase at the start of the
;; sentence, so no sentence would parse.

(define env (apply amb-environment parser-program))

(check (amb-collect '(parse '(the cat eats)) env)
       => '((sentence (simple-noun-phrase (article the) (noun cat))
                      (verb eats))))

(define (get-args operand-procs env succeed fail)
  (if (null? operand-procs)
      (succeed '() fail)
      (get-args (cdr operand-procs)
                env
                (lambda (args fail2)
                  ((car operand-procs)
                   env
                   (lambda (arg fail3)
                     (succeed (cons arg args) fail3))
                   fail2))
                fail)))

(check (amb-collect '(list (amb 1 2) (amb 'a 'b)) env)
       => '((1 a) (2 a) (1 b) (2 b)))
(check (amb-collect '(parse '(the cat eats)) env) => '())
