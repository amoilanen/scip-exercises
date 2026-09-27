(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "4.35.scm" (current-load-pathname)))

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
        5000
        (lambda () (amb-collect '(naive-pythagorean-triple) env 1)))
       => 'out-of-budget)

(check (amb-collect '(a-pythagorean-triple) env 4)
       => '((3 4 5) (6 8 10) (5 12 13) (9 12 15)))

