(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-record key data)
  (list key data))
(define (key-of-record record)
  (car record))
(define (data-of-record record)
  (cadr record))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
    false
    (let ((entry-key (key-of-record (entry set-of-records))))
      (cond
        ((= given-key entry-key) (data-of-record (entry set-of-records)))
        ((> given-key entry-key) (lookup given-key (right-branch set-of-records)))
        ((< given-key entry-key) (lookup given-key (left-branch set-of-records)))))))

(define tree-1
  (make-tree (make-record 3 "a")
    (make-tree (make-record 1 "b") '() '())
    (make-tree (make-record 7 "c")
      (make-tree (make-record 5 "d") '() '())
      (make-tree (make-record 9 "e") '()
        (make-tree (make-record 11 "f") '() '())))))

(newline)
(display (lookup 9 tree-1)) ; "e"

(newline)
(display (lookup 10 tree-1)) ; false