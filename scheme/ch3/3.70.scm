(load "lib/check.scm")
(load "ch3/lib/streams.scm")

;; Unlike merge, merge-weighted keeps both elements when their weights are
;; equal: different pairs may have the same weight.
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (<= (weight s1car) (weight s2car))
               (cons-stream s1car
                            (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream s2car
                            (merge-weighted s1 (stream-cdr s2) weight)))))))

;; Assumes weight grows along rows and columns, so that (s0 t0) is the
;; lightest pair and each part of the merge is already ordered.
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (pair-weight f)
  (lambda (pair) (f (car pair) (cadr pair))))

(define (ordered-by? weight items)
  (or (null? items)
      (null? (cdr items))
      (and (<= (weight (car items)) (weight (cadr items)))
           (ordered-by? weight (cdr items)))))

(define sum-weight (pair-weight +))

(define ordered-by-sum (weighted-pairs integers integers sum-weight))

(define (divisible-by-2-3-or-5? n)
  (or (= (remainder n 2) 0)
      (= (remainder n 3) 0)
      (= (remainder n 5) 0)))

(define coprime-to-30
  (stream-filter (lambda (n) (not (divisible-by-2-3-or-5? n))) integers))

(define weight-b
  (pair-weight (lambda (i j) (+ (* 2 i) (* 3 j) (* 5 i j)))))

(define ordered-by-weight-b
  (weighted-pairs coprime-to-30 coprime-to-30 weight-b))

(check (stream->list (merge-weighted (list->stream '((1 1) (2 2)))
                                     (list->stream '((1 2) (0 4)))
                                     sum-weight))
       => '((1 1) (1 2) (2 2) (0 4)))

(check (stream-head ordered-by-sum 6) => '((1 1) (1 2) (1 3) (2 2) (1 4) (2 3)))
(check (ordered-by? sum-weight (stream-head ordered-by-sum 300)) => #t)
(check (every (lambda (pair) (<= (car pair) (cadr pair)))
              (stream-head ordered-by-sum 300))
       => #t)

(check (stream-head ordered-by-weight-b 6)
       => '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19)))
(check (ordered-by? weight-b (stream-head ordered-by-weight-b 300)) => #t)
