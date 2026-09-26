(load "lib/check.scm")
(load "ch4/lib/query.scm")

;; An accumulation over the frames of a query counts a fact once for every
;; derivation of it, and rules such as wheel derive the same fact several
;; times (exercise 4.65).  Summing salaries of wheels adds Oliver's salary
;; four times.  The fix is to drop duplicate answers, i.e. frames that give
;; the same instantiation of the query, before accumulating.

(initialize-data-base! microshaft-data-base)

(define (answers-with-values variable query)
  (let ((pattern (query-syntax-process (list variable query))))
    (stream->list
     (stream-map (lambda (frame)
                   (instantiate pattern
                                frame
                                (lambda (var frame)
                                  (error "Unbound accumulation variable"
                                         (contract-question-mark var)))))
                 (query-frames (cadr pattern))))))

(define (distinct items)
  (fold-right (lambda (item result)
                (if (member item result)
                    result
                    (cons item result)))
              '()
              items))

(define (accumulate-over-query combine initial variable query)
  (fold-left combine initial (map car (answers-with-values variable query))))

(define (accumulate-over-distinct-answers combine initial variable query)
  (fold-left combine
             initial
             (map car (distinct (answers-with-values variable query)))))

(define wheel-salaries
  '(and (wheel ?who) (salary ?who ?amount)))

(check (accumulate-over-query + 0 '?amount wheel-salaries) => 660000)
(check (accumulate-over-distinct-answers + 0 '?amount wheel-salaries)
       => 210000)

(define programmer-salaries
  '(and (job ?who (computer programmer)) (salary ?who ?amount)))

(check (accumulate-over-query + 0 '?amount programmer-salaries) => 75000)
(check (accumulate-over-distinct-answers + 0 '?amount programmer-salaries)
       => 75000)
(check (accumulate-over-distinct-answers max 0 '?amount wheel-salaries)
       => 150000)
