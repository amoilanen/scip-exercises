(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/amb.scm" (current-load-pathname)))

;; Queens are placed column by column, and each new queen is checked against
;; the ones already placed right away, so a partial board that cannot be
;; completed is abandoned early.

(define env
  (amb-environment
   '(define (an-integer-between low high)
      (require (<= low high))
      (amb low (an-integer-between (+ low 1) high)))
   '(define (safe? row placed)
      (define (safe-from? others distance)
        (or (null? others)
            (let ((other (car others)))
              (and (not (= other row))
                   (not (= (abs (- other row)) distance))
                   (safe-from? (cdr others) (+ distance 1))))))
      (safe-from? placed 1))
   '(define (queens board-size)
      (define (place-from column placed)
        (if (> column board-size)
            (reverse placed)
            (let ((row (an-integer-between 1 board-size)))
              (require (safe? row placed))
              (place-from (+ column 1) (cons row placed)))))
      (place-from 1 '()))))

(check (amb-collect '(queens 1) env) => '((1)))
(check (amb-collect '(queens 3) env) => '())
(check (amb-collect '(queens 4) env) => '((2 4 1 3) (3 1 4 2)))
(check (length (amb-collect '(queens 5) env)) => 10)
(check (amb-collect '(queens 8) env 1) => '((1 5 8 6 3 7 2 4)))
