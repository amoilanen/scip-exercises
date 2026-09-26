(load "lib/check.scm")
(load "ch4/lib/query.scm")

(define (singleton-stream? s)
  (and (stream-pair? s)
       (stream-null? (stream-cdr s))))

(define (unique-query operands) (car operands))

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((answers (qeval (unique-query operands) (singleton-stream frame))))
       (if (singleton-stream? answers)
           answers
           the-empty-stream)))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)

(initialize-data-base! microshaft-data-base)

(check (run-query '(unique (job ?x (computer wizard))))
       => '((unique (job (Bitdiddle Ben) (computer wizard)))))
(check (run-query '(unique (job ?x (computer programmer)))) => '())

(check (run-query '(and (job ?x ?j) (unique (job ?anyone ?j))))
       (=> same-elements?)
       '((and (job (Bitdiddle Ben) (computer wizard))
              (unique (job (Bitdiddle Ben) (computer wizard))))
         (and (job (Tweakit Lem E) (computer technician))
              (unique (job (Tweakit Lem E) (computer technician))))
         (and (job (Reasoner Louis) (computer programmer trainee))
              (unique (job (Reasoner Louis) (computer programmer trainee))))
         (and (job (Warbucks Oliver) (administration big wheel))
              (unique (job (Warbucks Oliver) (administration big wheel))))
         (and (job (Scrooge Eben) (accounting chief accountant))
              (unique (job (Scrooge Eben) (accounting chief accountant))))
         (and (job (Cratchet Robert) (accounting scrivener))
              (unique (job (Cratchet Robert) (accounting scrivener))))
         (and (job (Aull DeWitt) (administration secretary))
              (unique (job (Aull DeWitt) (administration secretary))))))

(define supervisors-of-one-person
  '(and (supervisor ?person ?boss)
        (unique (supervisor ?anyone ?boss))))

(check (map (lambda (answer) (caddr (cadr answer)))
            (run-query supervisors-of-one-person))
       (=> same-elements?)
       '((Hacker Alyssa P) (Scrooge Eben)))
