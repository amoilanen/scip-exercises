(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a)
                (combiner (term a)
                          (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else   (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (inc x) (+ 1 x))
(define (id x) x)

; "a" part of the excercise
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(sum-squares-of-primes 2 6)

; "b" part of the excercise
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-smaller-relatively-primes n)
  (define (is-relatively-prime? k)
    (= (gcd n k) 1))
  (filtered-accumulate * 1 id 2 inc (- n 1) is-relatively-prime?))

(product-of-smaller-relatively-primes 7)