(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

;; Data paths of the iterative factorial:
;;
;;   registers   n, product, counter
;;   operations  mul: product * counter,  add: counter + 1,
;;               test >: counter > n
;;   buttons     p<-1 (constant 1 into product),  p<-mul (mul into product),
;;               c<-1 (constant 1 into counter),  c<-add (add into counter)
;;
;; Both mul and add read counter, and mul also reads product; the test reads
;; counter and n.
;;
;; Controller: push p<-1 and c<-1, then loop: if the test > holds, stop;
;; otherwise push p<-mul, then c<-add (in this order, since mul uses the old
;; counter), and go back to the test.

(define factorial-iter-controller
  '((assign product (const 1))
    (assign counter (const 1))
    test-counter
      (test (op >) (reg counter) (reg n))
      (branch (label fact-done))
      (assign product (op *) (reg product) (reg counter))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label test-counter))
    fact-done))

(define (factorial n)
  (run-machine (make-machine '(n product counter)
                             arithmetic-operations
                             factorial-iter-controller)
               (list (list 'n n))
               'product))

(check (factorial 0) => 1)
(check (factorial 1) => 1)
(check (factorial 5) => 120)
(check (factorial 10) => 3628800)
