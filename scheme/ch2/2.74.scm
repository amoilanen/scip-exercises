(define get 2d-get)

(define put 2d-put!)

(define record-files '())

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
  (define (get-salary name records)
    (let ((record (get-record name records)))
      (if (equal? record #f)
        #f
        (cdr (cadr (contents record))))))
  (put 'records 'boston (tag boston-records))
  (put 'get-record 'boston (lambda (name records)
    (get-record name (contents records))))
  (put 'get-salary 'boston (lambda (name records)
    (get-salary name (contents records))))
  (set! record-files (cons (tag boston-records) record-files))
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
  (define (get-salary name records)
    (let ((record (get-record name records)))
      (if (equal? record #f)
        #f
        (cdr (cdr (contents record))))))
  (put 'records 'tampa (tag tampa-records))
  (put 'get-record 'tampa (lambda (name records)
    (get-record name (contents records))))
  (put 'get-salary 'tampa (lambda (name records)
    (get-salary name (contents records))))
  (set! record-files (cons (tag tampa-records) record-files))
'done)

(define (install-seattle-records)
  (define (tag record)
    (attach-tag 'seattle record))
  (define seattle-records
    (map tag (list (cons (cons 'name "megan") (cons 'compensation 2.0)) (cons (cons 'name "lauren") (cons 'compensation 1.5)) (cons (cons 'name "jake") (cons 'compensation 1.5)))))
  (define (get-record name records)
    (cond ((null? records) #f)
      ((equal? (cdr (car (contents (car records)))) name) (car records))
      (else (get-record name (cdr records)))))
  (define (get-salary name records)
    (let ((record (get-record name records)))
      (if (equal? record #f)
        #f
        (cdr (cdr (contents record))))))
  (put 'records 'seattle (tag seattle-records))
  (put 'get-record 'seattle (lambda (name records)
    (get-record name (contents records))))
  (put 'get-salary 'seattle (lambda (name records)
    (get-salary name (contents records))))
  (put 'record-files 'seattle (lambda (name records)
    (get-salary name (contents records))))
  (set! record-files (cons (tag seattle-records) record-files))
'done)

(define (get-record name records)
  (contents ((get 'get-record (type-tag records)) name records)))

(define (get-salary name records)
  ((get 'get-salary (type-tag records)) name records))

(define (find-employee-record name record-files)
  (if (null? record-files) #f
    (let ((found-record ((get 'get-record (type-tag (car record-files))) name (car record-files))))
      (if (equal? found-record #f)
        (find-employee-record name (cdr record-files))
        found-record))))

(install-boston-records)
(install-tampa-records)
(install-seattle-records)

(define boston-records (get 'records 'boston))
(define tampa-records (get 'records 'tampa))
(define seattle-records (get 'records 'seattle))

(newline)
(display (get-record "margaret" boston-records))
(newline)
(display (get-salary "margaret" boston-records))

(newline)
(display (get-record "joe" tampa-records))
(newline)
(display (get-salary "joe" tampa-records))

(newline)
(display (get-record "megan" seattle-records))
(newline)
(display (get-salary "megan" seattle-records))

(newline)
(display (find-employee-record "rhys" record-files))

; a. Individual division files might be quite free form, however the file should be tagged with the department name
; b. Individual records inside division files need to be tagged with the division name
; d. When a new division/company is added its records file should be installed and register for itself three objects:
; 'records', 'get-record' and 'get-salary' and also append its records to the global list of record files