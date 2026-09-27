(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/amb.scm" (current-load-pathname)))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (value-proc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (value-proc env
                  (lambda (value fail2)
                    (set-variable-value! var value env)
                    (succeed 'ok fail2))
                  fail))))

(install-special-form! 'permanent-set! analyze-permanent-assignment)

(define (count-pairs assignment)
  `(let ((x (an-element-of '(a b c)))
         (y (an-element-of '(a b c))))
     (,assignment count (+ count 1))
     (require (not (eq? x y)))
     (list x y count)))

;; Every pair tried is counted, including (a a), which is rejected.  With
;; set! the increments are undone on backtracking, so count is always 1.
(check (amb-collect (count-pairs 'permanent-set!)
                    (amb-environment '(define count 0))
                    2)
       => '((a b 2) (a c 3)))
(check (amb-collect (count-pairs 'set!) (amb-environment '(define count 0)) 2)
       => '((a b 1) (a c 1)))

(let ((env (amb-environment '(define count 0))))
  (check (length (amb-collect (count-pairs 'permanent-set!) env)) => 6)
  (check (amb-collect 'count env) => '(9)))
