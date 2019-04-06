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

(define (unique-triples n)
  (flatmap
    (lambda (i)
      (flatmap
        (lambda (j)
          (map
            (lambda (k) (list i j k))
            (enumerate-interval 1 (- j 1))))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (triple-sum triple)
  (+ (car triple) (cadr triple) (caddr triple)))

(define (triples-with-sum n s)
  (filter
    (lambda (triple) (= s (triple-sum triple)))
    (unique-triples n)))

(newline)
(display (triples-with-sum 4 6))