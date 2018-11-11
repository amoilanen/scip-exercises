(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (inc-by inc)
  (lambda (x) (+ x inc)))

(define inc-by-2 (inc-by 2))

(define (pi-approximation n)
  (define (term k)
    (/ (square k) (square (- k 1))))
  (define (pi-by-four n)
    (/ (* 2 (product term 4 inc-by-2 n)) n))
  (exact->inexact (* 4 (pi-by-four n))))

(pi-approximation 10000)