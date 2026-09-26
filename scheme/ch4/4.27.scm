(load "lib/check.scm")
(load "ch4/lib/lazy.scm")

;; (define w (id (id 10))) applies the outer id at once, since the operator
;; is forced, so count becomes 1.  Its argument (id 10) is only delayed, and
;; the outer id returns it unforced: w is bound to that thunk.  Printing w
;; forces the thunk, which runs the inner id: w is 10 and count is now 2.
;;
;;   count  ;=> 1
;;   w      ;=> 10
;;   count  ;=> 2

(define definitions
  '((define count 0)
    (define (id x)
      (set! count (+ count 1))
      x)
    (define w (id (id 10)))))

(define (run . exps)
  (apply interpret (append definitions exps)))

(check (run 'count) => 1)
(check (run 'w) => 10)
(check (run 'count 'w 'count) => 2)
(check (run 'w 'w 'count) => 2)
