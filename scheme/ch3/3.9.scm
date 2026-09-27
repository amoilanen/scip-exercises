(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

;; Recursive version: (factorial 6) creates six frames E1..E6, each enclosed
;; by the global environment and binding n to 6, 5, 4, 3, 2 and 1.
;;
;; Iterative version: (factorial 6) creates E1 binding n to 6, then each call
;; of fact-iter creates a new frame enclosed by the global environment:
;;   E2: product 1,   counter 1, max-count 6
;;   E3: product 1,   counter 2, max-count 6
;;   E4: product 2,   counter 3, max-count 6
;;   E5: product 6,   counter 4, max-count 6
;;   E6: product 24,  counter 5, max-count 6
;;   E7: product 120, counter 6, max-count 6
;;   E8: product 720, counter 7, max-count 6
;;
;; The checks record the bindings of every frame as it is created.

(define frames '())

(define (record-frame! . bindings)
  (set! frames (cons bindings frames)))

(define (recorded-frames thunk)
  (set! frames '())
  (thunk)
  (reverse frames))

(define (factorial n)
  (record-frame! n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(check (factorial 6) => 720)
(check (recorded-frames (lambda () (factorial 6)))
       => '((6) (5) (4) (3) (2) (1)))

(define (factorial-iterative n)
  (record-frame! n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (record-frame! product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(check (factorial-iterative 6) => 720)
(check (recorded-frames (lambda () (factorial-iterative 6)))
       => '((6)
            (1 1 6) (1 2 6) (2 3 6) (6 4 6) (24 5 6) (120 6 6) (720 7 6)))
