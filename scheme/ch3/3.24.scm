(load "lib/check.scm")

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (and subtable
             (let ((record (assoc key-2 (cdr subtable))))
               (and record (cdr record))))))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (close-enough? a b)
  (< (abs (- a b)) 0.1))

(define grid (make-table close-enough?))
(define get (grid 'lookup-proc))
(define put (grid 'insert-proc!))

(put 1 2 'a)
(put 3 4 'b)
(check (get 1 2) => 'a)
(check (get 1.05 1.95) => 'a)
(check (get 2.95 4.01) => 'b)
(check (get 1.2 2) => #f)
(check (get 1 4) => #f)

(put 1.02 2.03 'c)
(check (get 1 2) => 'c)

(define strict (make-table equal?))
((strict 'insert-proc!) "math" "+" 43)
(check ((strict 'lookup-proc) "math" "+") => 43)
(check ((strict 'lookup-proc) "math" "-") => #f)
(check-error (strict 'delete!))
