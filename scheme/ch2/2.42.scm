(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ 1 low) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (inc x)
  (lambda (y) (+ x y)))

; (3 2 1) for board of 3 rows and 3 columns means:
; first row queen is in the 3rd column
; second row queen is in the 2nd column
; third row queen in the first column
(define empty-board ())

(define (adjoin-position new-queen rest-of-queens)
  (cons new-queen rest-of-queens))

(define (safe? positions)
  (let ((new-queen-column (car positions)))
    (= 0
       (length
          (filter
            (lambda (column) (= column new-queen-column))
            (cdr positions))))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-queen)
                   (adjoin-position new-queen rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (board-visualization board-size positions)
  (map
    (lambda (queen-column)
      (map
        (lambda (column)
          (if (= queen-column column) "x" "*"))
        (enumerate-interval 1 board-size)))
    positions))

(define (display-board board)
  (newline)
  (for-each
    (lambda (row)
      (newline)
      (display row))
    board))

(newline)
(display (safe? (list 2 1 3)))

(newline)
(for-each
  (lambda (positions)
    (display-board (board-visualization 3 positions)))
  (queens 3))

; 2.43 In the second version of the program all the possible queen combinations are checked rather than building on top
; of previously found solutions.
; T_n - faster solution time for n
; T_n = T_n-1 * n ~ n!
; G_n ~ n ^ n