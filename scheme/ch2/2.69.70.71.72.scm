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
          (successive-merge
            (adjoin-set
              (make-node rarest-leaf next-rarest-leaf)
              rest-of-leafs)))))))

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

; 2.71 the tree will be skewed to one side, n bits to encode the least frequent symbol, 1 to encode the most frequent one
; this is because sum(0..k, 2^k) = 2^(k+1) - 1 or all the symbols before next one has less summary frequency than the next symbol

; 2.72 O(n^2) because ~n symbol set to check at each node, and at most n nodes, however might be too pessimistic and estimate,
; only an upper bound.
; For the least frequent symbol all the nodes of the tree will be visited n + (n - 1) + (n - 2) + ... + 1 symbols to search in in every node
; = theta(n^2), however since the least frequent symbol is the first in the corresponding symbol sets, real time complexity 1 + 1 + ... + 1 = theta(n)
; For the most frequent symbol also theta(n) as we would have to search for it in the symbol set, it will be the last one there and only one tree node will be visited