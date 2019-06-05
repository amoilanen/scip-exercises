;2.64
;
; a. "divide and conquer" algorithm that splits the original problem into two subproblems of equal size, it is important that the list of elements
; elts is sorted for the tree to have the ordering property (all elements in the left subtree are less and the right greater than the current element)
;
; b. Every element is appened to the resulting tree once => O(n) complexity where n is the size of the list

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; has complexity Θ(n), every element is handled only once
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; has complexity Θ(n), every element is handled only once
(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
  (car (partial-tree elements (length elements))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (intersection-ordered-list-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-ordered-list-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-ordered-list-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-ordered-list-set set1 (cdr set2)))))))

(define (union-ordered-list-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
            (cond
              ((= x1 x2)
                (cons x1
                     (union-ordered-list-set (cdr set1)
                                (cdr set2))))
              ((< x1 x2)
                (cons x1 (union-ordered-list-set (cdr set1) set2)))
              ((< x2 x1)
                (cons x2 (union-ordered-list-set set1 (cdr set2)))))))))


(define (apply-list-set-op list-set-op set1 set2)
  (list->tree
    (list-set-op
      (tree->list set1)
      (tree->list set2))))

(define (intersection-set set1 set2)
  (apply-list-set-op intersection-ordered-list-set set1 set2))

(define (union-set set1 set2)
  (apply-list-set-op union-ordered-list-set set1 set2))

(define (show-set set)
  (display (tree->list set)))

(define s1 (list->tree (list 1 2 3)))
(define s2 (list->tree (list 2 3 4 5)))

(newline)
(show-set (adjoin-set 4 s1)) ; 1 2 3 4

(newline)
(show-set (adjoin-set 2 s1)) ; 1 2 3

(newline)
(show-set (adjoin-set 1 s2)) ; 1 2 3 4 5

(newline)
(show-set (intersection-set s1 s2)) ; 2 3

(newline)
(show-set (union-set s1 s2)) ; 1 2 3 4 5