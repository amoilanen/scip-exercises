(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ 1 x))

; 16 + 5 = 21
; (double (double double)) = (double (double ∘ double)) = (double ∘ double) ∘ (double ∘ double) = double ∘ double ∘ double ∘ double
; or doubling 4 times which is 2 ^ 4 = 16
(display
  (((double (double double)) inc) 5))