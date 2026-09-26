(load "lib/check.scm")

;; A table is a tree of entries (key value . children).  Any entry can hold
;; both a value and children, so (a) and (a b) can both be keys of the same
;; table.

(define no-value (list 'no-value))

(define (make-entry key) (list key no-value))
(define (entry-value entry) (cadr entry))
(define (entry-children entry) (cddr entry))
(define (set-entry-value! entry value) (set-car! (cdr entry) value))
(define (add-child! entry child)
  (set-cdr! (cdr entry) (cons child (entry-children entry))))

(define (make-table) (make-entry '*table*))

(define (child entry key)
  (assoc key (entry-children entry)))

(define (lookup keys table)
  (let loop ((entry table) (keys keys))
    (cond ((not entry) #f)
          ((pair? keys) (loop (child entry (car keys)) (cdr keys)))
          ((eq? (entry-value entry) no-value) #f)
          (else (entry-value entry)))))

(define (insert! keys value table)
  (define (child! entry key)
    (or (child entry key)
        (let ((new-entry (make-entry key)))
          (add-child! entry new-entry)
          new-entry)))
  (let loop ((entry table) (keys keys))
    (if (pair? keys)
        (loop (child! entry (car keys)) (cdr keys))
        (set-entry-value! entry value)))
  'ok)

(define t (make-table))
(insert! '(math +) 43 t)
(insert! '(math -) 45 t)
(insert! '(letters a) 97 t)
(insert! '(a) 1 t)
(insert! '(a b c) 3 t)
(insert! '(a b) 2 t)
(insert! '("str" (1 2)) 'compound-keys t)

(check (lookup '(math +) t) => 43)
(check (lookup '(math -) t) => 45)
(check (lookup '(letters a) t) => 97)
(check (lookup '(a) t) => 1)
(check (lookup '(a b) t) => 2)
(check (lookup '(a b c) t) => 3)
(check (lookup '("str" (1 2)) t) => 'compound-keys)

(check (lookup '(math) t) => #f)
(check (lookup '(math *) t) => #f)
(check (lookup '(a b c d) t) => #f)
(check (lookup '(z) t) => #f)

(insert! '(math +) 'plus t)
(check (lookup '(math +) t) => 'plus)
(check (lookup '(math -) t) => 45)
