(define (repeated f times)
  (if (= times 1) f
      (lambda (x)
        (f
          ((repeated f (- times 1)) x)))))

(display ((repeated square 2) 5))