(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum-of-squares from to)
  (sum square from (lambda (x) (+ x 1)) to))

(sum-of-squares 3 4)