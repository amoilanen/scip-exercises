(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (column-to-row v)
  (accumulate (lambda (cur acc) (cons (car cur) acc)) () v))

(define (row-to-column v)
  (map list v))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (let ((v-as-row (column-to-row v)))
    (map (lambda (m-row) (list (dot-product m-row v-as-row))) m)))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row-of-m)
      (map (lambda (col-of-n)
        (dot-product row-of-m col-of-n))
      cols))
      m)))

; 1 2 3
; 4 5 6
(define m1 (list (list 1 2 3) (list 4 5 6)))

; 1 2 3
; 4 5 6
; 7 8 9
(define m2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; 1
; 2
; 3
(define v (list (list 1) (list 2) (list 3)))

; 1 2 3
(define v1 (list 1 2 3))

; 4 5 6
(define v2 (list 4 5 6))

; 32
(newline)
(display (dot-product v1 v2))

; 1 2 3
(newline)
(display (column-to-row v))

; 1
; 2
; 3
(newline)
(display (row-to-column v1))

; 14
; 32
(newline)
(display (matrix-*-vector m1 v))

; 1 4
; 2 5
; 3 6
(newline)
(display (transpose m1))

; 30 36 42
; 66 81 96
(newline)
(display (matrix-*-matrix m1 m2))