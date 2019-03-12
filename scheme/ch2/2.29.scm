(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; Alternative definitions of make-mobile and make-branch using cons instead of list
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-branch-weight branch)
  (if (number? (branch-structure branch))
    (branch-structure branch)
    (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (total-branch-weight (left-branch mobile)) (total-branch-weight (right-branch mobile))))

(define (is-balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (total-branch-weight branch)))
  (define (is-balanced-branch? branch)
    (if (number? (branch-structure branch))
      #t
      (is-balanced? (branch-structure branch))))
  (and
    (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
    (is-balanced-branch? (left-branch mobile))
    (is-balanced-branch? (right-branch mobile))))

;    |
;  |   |
; | 3 4 5
;1 2
(define x
  (make-mobile
    (make-branch 1
      (make-mobile
        (make-branch 1
          (make-mobile
            (make-branch 1 1)
            (make-branch 1 2)))
        (make-branch 1 3)))
    (make-branch 1
      (make-mobile
        (make-branch 1 4)
        (make-branch 1 5)))))

(newline)
(display (total-weight x))

;         |
;    |(2)     |(3)
; |(1) |(2)   2
; 2    1
(define y
  (make-mobile
    (make-branch 2
      (make-mobile
        (make-branch 1 2)
        (make-branch 2 1)))
    (make-branch 3 2)))

;    |
; |(2) |(3)
; 2    3
(define z
  (make-mobile
    (make-branch 2 2)
    (make-branch 3 2)))

(newline)
(display (is-balanced? y))

(newline)
(display (is-balanced? z))

