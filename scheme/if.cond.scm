; Defining if via cond
(define (if p x y)
  (cond (p x) ((not p) y)))

;Defining cond via if
(define (cond . checks)
  (cond-list checks))

(define (cond-list checks)
  (if
    (car (car checks))
    (cdr (car checks))
    (cond-list (cdr checks))))

(define (case x y)
  (list x y))

(if (> 2 1) "2 > 1 is true" "2 > 1 is false")
(cond (case (> 1 2) "1 > 2") (case (> 2 2) "2 > 2") (case (< 1 2) "1 < 2") )
