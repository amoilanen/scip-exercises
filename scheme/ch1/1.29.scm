(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-rule-integral f a b n)
  (define dx (/ (- b a) n))
  (define (next-idx idx) (+ 1 idx))
  (define (f-at-idx idx) (f (+ a (* idx dx))))
  (define (term idx)
    (cond ((or (= idx 0) (= idx n)) (f-at-idx idx))
          ((= (remainder idx 2) 1) (* 4 (f-at-idx idx)))
          (else (* 2 (f-at-idx idx)))))
  (* (/ dx 3) (sum term 0 next-idx n)))

(define (cube x) (* x x x))

(
  (integral cube 0 1 0.01)
  (simpsons-rule-integral cube 0 1 100)
)