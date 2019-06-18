(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf) (cadr leaf))

(define (weight-leaf leaf) (caddr leaf))

(define (contains? el list)
  (not (eq? false (member el list))))

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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (let ((leaf-set-size (length leaf-set)))
    (cond
      ((eq? 0 leaf-set-size) '())
      ((eq? 1 leaf-set-size) (car leaf-set))
      (else
        (let ((rarest-leaf (car leaf-set))
              (next-rarest-leaf (cadr leaf-set))
              (rest-of-leafs (cddr leaf-set)))
          (successive-merge (adjoin-set (make-node rarest-leaf next-rarest-leaf) rest-of-leafs)))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-subtree symbol bits tree)
    (cond ((null? tree) (error "unknown symbol -- cannot encode" symbol))
        ((leaf? tree) bits)
        ((contains? symbol (symbols (left-branch tree)))
          (encode-symbol-subtree symbol (cons 0 bits) (left-branch tree)))
        ((contains? symbol (symbols (right-branch tree)))
          (encode-symbol-subtree symbol (cons 1 bits) (right-branch tree)))
        (else (error "unknown symbol - cannot encode" symbol))))
  (reverse (encode-symbol-subtree symbol '() tree)))

(define symbol-frequencies '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define huffman-code-tree (generate-huffman-tree symbol-frequencies))
(display huffman-code-tree)

(define song '(Get a job

Sha na na na na na na na na

Get a job

Sha na na na na na na na na

Wah yip yip yip yip yip yip yip yip yip

Sha boom))

(define encoded-song (encode song huffman-code-tree))

(newline)
(display encoded-song)

(newline)
(display "Huffman encoding required bits:")
(newline)
(display (length encoded-song))

(newline)
(display "Fixed encoding required bits:")
(newline)
(display (* 3 (length song)))

(newline)
(display "Saved space, %")
(newline)
(display (exact->inexact (- 100 (* 100 (/ (length encoded-song) (* 3 (length song)))))))