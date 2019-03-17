; Recursive definition.
; Subsets of the current non-empty set:
; - all subsets without some chosen element x
; - all subsets that include element x
; If set is empty, subsets are an empty set themselves
(define (subsets s)
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest
        (map
          (lambda (subset)
            (cons (car s) subset))
          rest)))))

(newline)
(display (subsets (list 1 2 3)))