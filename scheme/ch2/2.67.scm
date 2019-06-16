(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf) (cadr leaf))

(define (weight-leaf leaf) (caddr leaf))

(define (make-node left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch node) (car node))

(define (right-branch node) (cadr node))

(define (symbols-node node) (caddr node))

(define (weight-node node) (cadddr node))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (symbols-node tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (weight-node tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-node (make-leaf 'A 4)
                  (make-node
                    (make-leaf 'B 2)
                    (make-node (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree))