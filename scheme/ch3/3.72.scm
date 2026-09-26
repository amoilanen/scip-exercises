(load "lib/check.scm")
(load "ch3/3.70.scm")

(define sum-of-squares
  (pair-weight (lambda (i j) (+ (square i) (square j)))))

;; Turns a stream ordered by weight into a stream of (weight element ...),
;; one list per run of elements with that weight.
(define (group-by-weight s weight)
  (if (stream-null? s)
      the-empty-stream
      (let ((w (weight (stream-car s))))
        (define (same-weight? rest)
          (and (not (stream-null? rest))
               (= (weight (stream-car rest)) w)))
        (let collect ((rest (stream-cdr s)) (group (list (stream-car s))))
          (if (same-weight? rest)
              (collect (stream-cdr rest) (cons (stream-car rest) group))
              (cons-stream (cons w (reverse group))
                           (group-by-weight rest weight)))))))

(define sums-of-squares-three-ways
  (stream-filter
   (lambda (group) (>= (length (cdr group)) 3))
   (group-by-weight (weighted-pairs integers integers sum-of-squares)
                    sum-of-squares)))

(check (stream->list
        (group-by-weight (list->stream '(1 3 3 4 4 4 5)) square))
       => '((1 1) (9 3 3) (16 4 4 4) (25 5)))
(check (stream-head sums-of-squares-three-ways 3)
       => '((325 (1 18) (6 17) (10 15))
            (425 (5 20) (8 19) (13 16))
            (650 (5 25) (11 23) (17 19))))
