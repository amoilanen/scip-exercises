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
  (define (get-records-list records-file)
    records-file)
  (define (get-record-name record)
    (car record))
  (define (get-record-salary record)
    (cdr (cadr record)))
  (put 'records-file 'boston (tag boston-records))
  (put 'get-records-list 'boston
    (lambda (records-file) (get-records-list (contents records-file))))
  (put 'get-record-name 'boston (lambda (record)
    (get-record-name (contents record))))
  (put 'get-record-salary 'boston (lambda (record)
    (get-record-salary (contents record))))
  (set! record-files (cons (tag boston-records) record-files))
'done)

(define (install-tampa-records)
  (define (tag record)
    (attach-tag 'tampa record))
  (define tampa-records
    (cons 'tampa-file (map tag (list (cons "kyle" (cons 'gross-salary 2.0)) (cons "joe" (cons 'gross-salary 2.0)) (cons "rhys" (cons 'gross-salary 1.9))))))
  (define (get-records-list records-file)
    (cdr records-file))
  (define (get-record-name record)
    (car record))
  (define (get-record-salary record)
    (cdr (cdr record)))
  (put 'records-file 'tampa (tag tampa-records))
  (put 'get-records-list 'tampa
    (lambda (records-file) (get-records-list (contents records-file))))
  (put 'get-record-name 'tampa (lambda (record)
    (get-record-name (contents record))))
  (put 'get-record-salary 'tampa (lambda (record)
    (get-record-salary (contents record))))
  (set! record-files (cons (tag tampa-records) record-files))
'done)

(define (install-seattle-records)
  (define (tag record)
    (attach-tag 'seattle record))
  (define seattle-records
    (list 'seattle-division 'v0.9
          (map tag (list (cons (cons 'name "megan") (cons 'compensation 2.0)) (cons (cons 'name "lauren") (cons 'compensation 1.5)) (cons (cons 'name "jake") (cons 'compensation 1.5))))))
  (define (get-records-list records-file)
    (caddr records-file))
  (define (get-record-name record)
    (cdr (car record)))
  (define (get-record-salary record)
    (cdr (cdr record)))
  (put 'records-file 'seattle (tag seattle-records))
  (put 'get-records-list 'seattle
    (lambda (records-file) (get-records-list (contents records-file))))
  (put 'get-record-name 'seattle (lambda (record)
    (get-record-name (contents record))))
  (put 'get-record-salary 'seattle (lambda (record)
    (get-record-salary (contents record))))
  (set! record-files (cons (tag seattle-records) record-files))
'done)

(define (get-record-name record)
  (let ((get-name-method (get 'get-record-name (type-tag record))))
    (get-name-method record)))

(define (get-record-salary record)
  (let ((get-name-method (get 'get-record-salary (type-tag record))))
    (get-name-method record)))

(define (get-records-list records-file)
  (let ((get-records-list-method (get 'get-records-list (type-tag records-file))))
    (get-records-list-method records-file)))

; Generic methods implemented on top of get-record-name, get-record-salary and get-records-list specific for every installed division module

(define (get-record name records-file)
  (define (get-record-from-records-list name records)
    (cond ((null? records) #f)
    ((equal? (get-record-name (car records)) name) (car records))
    (else (get-record-from-records-list name (cdr records)))))
  (get-record-from-records-list name (get-records-list records-file)))

(define (get-salary name records-file)
  (let ((record (get-record name records-file)))
    (if (equal? record #f)
      #f
      (get-record-salary record))))

(define (find-employee-record name record-files)
  (if (null? record-files) #f
    (let ((current-record-file (car record-files)))
      (let ((found-record (get-record name current-record-file)))
        (if (eq? found-record #f)
          (find-employee-record name (cdr record-files))
          found-record)))))

(install-boston-records)
(install-tampa-records)
(install-seattle-records)

(define boston-records (get 'records-file 'boston))
(define tampa-records (get 'records-file 'tampa))
(define seattle-records (get 'records-file 'seattle))

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
(newline)
(display (get-record-salary (find-employee-record "rhys" record-files)))

; a. Individual division files might be quite free form, however every employee record should be tagged with the division name and every record file should expose a method get-records-list
; b. Individual records inside division files need to be tagged with the division name
; d. When a new division/company is added its records file should be installed and register for itself three methods:
; 'get-record-name', 'get-record-salary', and 'get-records-list' and also append its records to the global list of record files