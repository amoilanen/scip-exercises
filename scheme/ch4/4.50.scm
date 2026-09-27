(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "4.49.scm" (current-load-pathname)))

;; ramb-choice maps the number of remaining alternatives n to the index of
;; the one to try next, a number in [0, n).
(define ramb-choice (make-parameter random))

(define (analyze-ramb exp)
  (let ((choice-procs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (let try-next ((choices choice-procs))
        (if (null? choices)
            (fail)
            (let ((index ((ramb-choice) (length choices))))
              ((list-ref choices index)
               env
               succeed
               (lambda ()
                 (try-next (remove-index choices index))))))))))

(define (remove-index items index)
  (append (list-head items index) (list-tail items (+ index 1))))

(install-special-form! 'ramb analyze-ramb)

(define env (amb-environment))

(check (amb-collect '(ramb) env) => '())
(check (sort (amb-collect '(ramb 1 2 3 4 5) env) <) => '(1 2 3 4 5))
(check (parameterize ((ramb-choice (lambda (n) 0)))
         (amb-collect '(ramb 1 2 3 4 5) env))
       => '(1 2 3 4 5))
(check (parameterize ((ramb-choice (lambda (n) (- n 1))))
         (amb-collect '(ramb 1 2 3 4 5) env))
       => '(5 4 3 2 1))
(check (parameterize ((ramb-choice (lambda (n) (- n 1))))
         (amb-collect '(list (ramb 1 2) (ramb 'a 'b)) env))
       => '((2 b) (2 a) (1 b) (1 a)))

;; With ramb in place of amb when choosing words and deciding whether to
;; extend a phrase, the generator of exercise 4.49 no longer always starts
;; with the same sentence: every run makes its own choices, so the sentences
;; vary in both words and structure.  A retry still revises the most recent
;; choice first, though, so the sentences found by successive retries of one
;; run remain extensions of each other.

(define random-generator-program
  (append generator-program
          '((define (a-random-element-of items)
              (require (not (null? items)))
              (ramb (car items) (a-random-element-of (cdr items))))
            (define (parse-word word-list)
              (list (car word-list) (a-random-element-of (cdr word-list))))
            (define (parse-noun-phrase)
              (define (maybe-extend noun-phrase)
                (ramb noun-phrase
                      (maybe-extend (list 'noun-phrase
                                          noun-phrase
                                          (parse-prepositional-phrase)))))
              (maybe-extend (parse-simple-noun-phrase)))
            (define (parse-verb-phrase)
              (define (maybe-extend verb-phrase)
                (ramb verb-phrase
                      (maybe-extend (list 'verb-phrase
                                          verb-phrase
                                          (parse-prepositional-phrase)))))
              (maybe-extend (parse-word verbs))))))

;; A linear congruential generator makes the random choices reproducible.
(define (make-pseudo-random-choice seed)
  (lambda (n)
    (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
    (modulo (quotient seed 65536) n)))

(define (first-sentences runs)
  (let ((env (apply amb-environment random-generator-program)))
    (parameterize ((ramb-choice (make-pseudo-random-choice 2024)))
      (map (lambda (run)
             (sentence-words (car (amb-collect '(parse-sentence) env 1))))
           (iota runs)))))

(define (grammatical? words)
  (pair? (amb-collect `(parse ',words)
                      (apply amb-environment parser-program)
                      1)))

(let ((sentences (first-sentences 4)))
  (check (length (delete-duplicates sentences)) => 4)
  (check (every grammatical? sentences) => #t))
