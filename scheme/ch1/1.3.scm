(define (square x)
  (* x x))

(define (sum-squares x y)
  (+ (square x) (square y)))

; Procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers
(define (sum-two-largest-squares x y z)
  (if (> x y)
    (if (> z y)
      (sum-squares x z)
      (sum-squares x y))
    (if (> z x)
      (sum-squares y z)
      (sum-squares y x))))