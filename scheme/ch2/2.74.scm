(define get 2d-get)

(define put 2d-put!)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; Three divisions: Boston, Tampa, Seattle

(define (install-boston-records)
  (define (tag record)
    (attach-tag 'boston record))
  (define boston-records
    (map tag (list (list "jake" (cons 'salary 2.0)) (list "margaret" (cons 'salary 2.5)) (list "reece" (cons 'salary 1.5)))))
  (define (get-record name records)
    (cond ((null? records) #f)
      ((equal? (car (contents (car records))) name) (car records))
      (else (get-record name (cdr records)))))
  (put 'records 'boston (tag boston-records))
  (put 'get-record 'boston (lambda (name records)
    (get-record name (contents records))))
'done)

(define (install-tampa-records)
  (define (tag record)
    (attach-tag 'tampa record))
  (define tampa-records
    (map tag (list (cons "kyle" (cons 'gross-salary 2.0)) (cons "joe" (cons 'gross-salary 2.0)) (cons "rhys" (cons 'gross-salary 1.9)))))
  (define (get-record name records)
    (cond ((null? records) #f)
      ((equal? (car (contents (car records))) name) (car records))
      (else (get-record name (cdr records)))))
  (put 'records 'tampa (tag tampa-records))
  (put 'get-record 'tampa (lambda (name records)
    (get-record name (contents records))))
'done)

(define (install-seattle-records)
  (define (tag record)
    (attach-tag 'seattle record))
  (define seattle-records
    (map tag (list (cons (cons 'name "megan") (cons 'compensation 2.0)) (cons (cons 'name "lauren") (cons 'compensation 1.5)) (cons (cons 'name "jake") (cons 'compensation 1.5)))))
  (define (get-record name records)
    (cond ((null? records) #f)
      ((equal? (cadr (car (contents (car records)))) name) record)
      (else (get-record name (cdr records)))))
  (put 'records 'seattle (tag seattle-records))
  (put 'get-record 'seattle (lambda (name records)
    (get-record name (contents records))))
'done)

(install-boston-records)
(install-tampa-records)
(install-seattle-records)

(define records (get 'records 'boston))

(define (get-record name records)
  (contents ((get 'get-record (type-tag records)) name records)))

(display (get-record "margaret" records))