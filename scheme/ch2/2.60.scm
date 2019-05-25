(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

(define (uniq s)
  (cond
    ((null? s) s)
    ((element-of-set? (car s) (cdr s)) (uniq (cdr s)))
    (else (cons (car s) (uniq (cdr s))))))

(define s1 (list 2 3 2 1 3 2 2))
(define s2 (list 2 3 5 4 5 4))

(newline)
(display (uniq (adjoin-set 4 s1))) ; 4 1 2 3

(newline)
(display (uniq (adjoin-set 2 s1))) ; 1 2 3

(newline)
(display (uniq (intersection-set s1 s2))) ; 2 3

(newline)
(display (uniq (union-set s1 s2))) ; 1 2 3 4 5


; "How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation?"
; element-of-set is slower with the duplicate presentation, however still linear O(n)
; adjoin-set is faster with the duplicate presentation, constant time O(1)
; union-set is faster with the duplicate presentation, constant time O(1)
; intersection-set is slower with the duplicate presentation, however still linear O(n)

; element-of-set and intersection-set take a performance hit depending on the number of duplicate elements in the set
; ajoin-set and union-set are significantly faster


; "Are there applications for which you would use this representation in preference to the non-duplicate one?"
; Duplicate presentation is preferable when most of the operations are adding new elements and computing union of sets.