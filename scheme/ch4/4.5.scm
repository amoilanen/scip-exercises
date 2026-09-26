(load "lib/check.scm")
(load "ch4/lib/mceval.scm")

(define (cond-arrow-clause? clause)
  (and (pair? (cond-actions clause))
       (eq? (car (cond-actions clause)) '=>)))

(define (cond-recipient clause) (cadr (cond-actions clause)))

;; The test is evaluated once and its value is passed to the recipient. The
;; recipient and the remaining clauses are wrapped in thunks created in the
;; caller's environment, so the generated parameters cannot capture any of
;; their variables:
;;   ((lambda (value recipient otherwise)
;;      (if value ((recipient) value) (otherwise)))
;;    test
;;    (lambda () recipient-exp)
;;    (lambda () rest-of-cond))
(define (expand-arrow-clause clause rest)
  (list (make-lambda '(value recipient otherwise)
                     (list (make-if 'value
                                    '((recipient) value)
                                    '(otherwise))))
        (cond-predicate clause)
        (make-lambda '() (list (cond-recipient clause)))
        (make-lambda '() (list rest))))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF" clauses)))
              ((cond-arrow-clause? first)
               (expand-arrow-clause first (expand-clauses rest)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define primitive-procedures
  (cons (list 'assoc assoc) primitive-procedures))

(check (interpret '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
                         (else false)))
       => 2)
(check (interpret '(cond ((assoc 'c '((a 1) (b 2))) => cadr)
                         (else 'none)))
       => 'none)
(check (interpret '(cond ((assoc 'c '((a 1))) => cadr))) => #f)
(check (interpret '(cond ((= 1 2) 'first)
                         ((+ 1 2) => (lambda (x) (* x x)))
                         (else 'last)))
       => 9)
(check (interpret '(define tests 0)
                  '(cond ((begin (set! tests (+ tests 1)) tests) => -))
                  'tests)
       => 1)
(check (interpret '(define value 10)
                  '(define (otherwise) 'outer)
                  '(cond ((+ 1 1) => (lambda (x) (+ x value)))))
       => 12)
(check (interpret '(define (otherwise) 'outer)
                  '(cond (false => car)
                         (else (otherwise))))
       => 'outer)
