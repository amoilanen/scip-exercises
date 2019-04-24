; Dummy definitions of 'below' and 'beside'
(define (below x y) x)
(define (beside x y) x)

(define (split transform-painter transform-smaller)
  (lambda (painter n) (
    (if (= n 0)
      painter
      (let ((smaller ((split transform-painter transform-smaller) painter (- n 1))))
        (transform-painter painter (transform-smaller smaller smaller)))))))

(define right-split (split beside below))
(define up-split (split below beside))

; Dummy examples using a dummy lambda function instead of a painter
(right-split (lambda () 1) 10)
(up-split (lambda () 1) 10)