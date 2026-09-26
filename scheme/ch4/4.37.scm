(load "lib/check.scm")
(load "ch4/4.35.scm")

;; Ben is right.  The original procedure explores every triple i <= j <= k,
;; about n^3/6 of them for a range of n integers.  Ben's chooses only i and
;; j, about n^2/2 pairs, computes k and rejects a pair as soon as
;; i^2 + j^2 is too large or not a perfect square.

(define ben-env
  (apply amb-environment
         (append integer-between-program
                 '((define (a-pythagorean-triple-between low high)
                     (let ((i (an-integer-between low high))
                           (hsq (* high high)))
                       (let ((j (an-integer-between i high)))
                         (let ((ksq (+ (* i i) (* j j))))
                           (require (>= hsq ksq))
                           (let ((k (sqrt ksq)))
                             (require (integer? k))
                             (list i j k))))))))))

(define (triples-and-work env high)
  (let ((triples '()))
    (let ((work
           (count-applications
            (lambda ()
              (set! triples
                    (amb-collect `(a-pythagorean-triple-between 1 ,high)
                                 env))))))
      (cons triples work))))

(let ((original (triples-and-work env 15))
      (ben (triples-and-work ben-env 15)))
  (check (car ben) => (car original))
  (check (< (* 4 (cdr ben)) (cdr original)) => #t))
