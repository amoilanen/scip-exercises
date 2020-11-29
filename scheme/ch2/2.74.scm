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
  (define (get-record-name record)
    (car record))
  (define (get-record-salary record)
    (cdr (cadr record)))
  (put 'records 'boston boston-records)
  (put 'get-record-name 'boston (lambda (record)
    (get-record-name (contents record))))
  (put 'get-record-salary 'boston (lambda (record)
    (get-record-salary (contents record))))
  (set! record-files (cons boston-records record-files))
'done)

(define (install-tampa-records)
  (define (tag record)
    (attach-tag 'tampa record))
  (define tampa-records
    (map tag (list (cons "kyle" (cons 'gross-salary 2.0)) (cons "joe" (cons 'gross-salary 2.0)) (cons "rhys" (cons 'gross-salary 1.9)))))
  (define (get-record-name record)
    (car record))
  (define (get-record-salary record)
    (cdr (cdr record)))
  (put 'records 'tampa tampa-records)
  (put 'get-record-name 'tampa (lambda (record)
    (get-record-name (contents record))))
  (put 'get-record-salary 'tampa (lambda (record)
    (get-record-salary (contents record))))
  (set! record-files (cons tampa-records record-files))
'done)

(define (install-seattle-records)
  (define (tag record)
    (attach-tag 'seattle record))
  (define seattle-records
    (map tag (list (cons (cons 'name "megan") (cons 'compensation 2.0)) (cons (cons 'name "lauren") (cons 'compensation 1.5)) (cons (cons 'name "jake") (cons 'compensation 1.5)))))
  (define (get-record-name record)
    (cdr (car record)))
  (define (get-record-salary record)
    (cdr (cdr record)))
  (put 'records 'seattle seattle-records)
  (put 'get-record-name 'seattle (lambda (record)
    (get-record-name (contents record))))
  (put 'get-record-salary 'seattle (lambda (record)
    (get-record-salary (contents record))))
  (set! record-files (cons seattle-records record-files))
'done)

(define (get-record-name record)
  ;(display "get-record-name\n")
  ;(display record)
  ;(display "\n")
  (let ((get-name-method (get 'get-record-name (type-tag record))))
    ;(display (get-name-method record))
    ;(display "\n")
    (get-name-method record)))

(define (get-record-salary record)
  (let ((get-name-method (get 'get-record-salary (type-tag record))))
    (get-name-method record)))

(define (get-record name records)
  ;(display records)
  ;(display "\n")
  (cond ((null? records) #f)
    ((equal? (get-record-name (car records)) name) (car records))
    (else (get-record name (cdr records)))))

(define (get-salary name records)
  (let ((record (get-record name records)))
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
(newline)
(display (get-record-salary (find-employee-record "rhys" record-files)))

; a. Individual division files might be quite free form, however every employee record should be tagged with the division name and every record file should expose a method get-records-list
; b. Individual records inside division files need to be tagged with the division name
; d. When a new division/company is added its records file should be installed and register for itself three methods:
; 'get-record-name', 'get-record-salary', and 'get-records-list' and also append its records to the global list of record files