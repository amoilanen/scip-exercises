(load "lib/check.scm")
(load "ch4/4.35.scm")

;; Replacing an-integer-between by an-integer-starting-from does not work:
;; with i and j fixed at 1 the search backtracks only to the most recent
;; choice, k, which has infinitely many values, so i and j never change and
;; no triple is ever found.  Instead choose the largest number k first, from
;; the infinite range, and then i and j from the finite range below it.

(define env
  (apply amb-environment
         (append integer-between-program
                 '((define (naive-pythagorean-triple)
                     (let ((i (an-integer-starting-from 1)))
                       (let ((j (an-integer-starting-from i)))
                         (let ((k (an-integer-starting-from j)))
                           (require (= (+ (* i i) (* j j)) (* k k)))
                           (list i j k)))))
                   (define (a-pythagorean-triple)
                     (let ((k (an-integer-starting-from 1)))
                       (let ((i (an-integer-between 1 k)))
                         (let ((j (an-integer-between i k)))
                           (require (= (+ (* i i) (* j j)) (* k k)))
                           (list i j k)))))))))

(check (with-application-budget
        20000
        (lambda () (amb-collect '(naive-pythagorean-triple) env 1)))
       => 'out-of-budget)

(check (amb-collect '(a-pythagorean-triple) env 6)
       => '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17) (12 16 20)))

;; Each triple is found once: its k is chosen exactly once.
(check (let ((triples (amb-collect '(a-pythagorean-triple) env 20)))
         (= (length triples) (length (delete-duplicates triples))))
       => #t)
