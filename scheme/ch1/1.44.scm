(define dx 0.0001)

(define (repeated f times)
  (if (= times 1) f
      (lambda (x)
        (f
          ((repeated f (- times 1)) x)))))

(define (smooth f)
  (lambda (x)
    (let ((fx (f x))
          (mfx (f (- x dx)))
          (pfx (f (+ x dx))))
      (
        / (+ mfx fx pfx) 3))))

(define (smooth-n f n)
  ((repeated smooth n) f))

(define smoothed-square (smooth square))
(define n-smoothed-square (smooth-n square 10))

(newline)
(display (square 2))
(newline)
(display (smoothed-square 2))
(newline)
(display (n-smoothed-square 2))