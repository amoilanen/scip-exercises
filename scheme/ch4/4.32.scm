(load "lib/check.scm")
(load "ch4/lib/lazy.scm")

;; The streams of chapter 3 delay only the cdr; lazy lists delay the car as
;; well.  Two ways to exploit that:
;;
;; - Elements are computed only when they are needed, so mapping over a
;;   lazy list and picking one element runs the procedure once, whereas
;;   stream-map runs it for every element up to the one picked.
;; - An element may refer to later elements of its own list, or to the
;;   list itself, since it is not evaluated when the list is built.  The
;;   same goes for trees whose nodes are expensive or refer to each other.

(define (run . exps)
  (apply interpret (append lazy-list-definitions exps)))

(define counting-definitions
  '((define count 0)
    (define (id x)
      (set! count (+ count 1))
      x)))

(define (run-counting . exps)
  (apply run (append counting-definitions exps)))

(check (run-counting '(list-ref (map id integers) 5)) => 6)
(check (run-counting '(list-ref (map id integers) 5) 'count) => 1)

(define (stream-map-count)
  (let ((count 0))
    (define (id x)
      (set! count (+ count 1))
      x)
    (define (integers-from n)
      (cons-stream n (integers-from (+ n 1))))
    (stream-ref (stream-map id (integers-from 1)) 5)
    count))

(check (stream-map-count) => 6)

(check (run '(define xs (cons (+ (car (cdr xs)) 1)
                              (cons 10 '())))
            '(car xs))
       => 11)

(define (self-referencing-stream)
  (define xs
    (cons-stream (+ (stream-car (stream-cdr xs)) 1)
                 (cons-stream 10 '())))
  xs)

(check-error (self-referencing-stream))
