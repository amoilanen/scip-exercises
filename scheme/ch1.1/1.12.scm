(define (p n m)
  (if (or (= m 1) (= m n))
      1
      (+ (p (- n 1) m) (p (- n 1) (- m 1)))))

(define (pascal-row n)
  (define (generate-row acc n m)
    (if (> m n)
        acc
        (generate-row (cons (p n m) acc)
                      n
                      (+ m 1))))
  (generate-row '() n 1))

(define (pascal-triangle n)
  (define (generate-rows acc m)
    (if (= m 0)
        acc
        (generate-rows (cons (pascal-row m) acc) (- m 1))))
  (generate-rows '() n))