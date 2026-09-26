(load "lib/check.scm")
(load "ch5/5.43.scm")
(load "ch5/lib/open-coding.scm")

;; An operator that names a primitive is open-coded only if it isn't bound
;; by an enclosing lambda, i.e. if it isn't in the compile-time environment.

(define open-coded-in-any-scope? open-coded?)

(define (open-coded? exp)
  (and (open-coded-in-any-scope? exp)
       (eq? (find-variable (operator exp) (compile-time-environment))
            'not-found)))

(define linear-combination
  '(define (linear-combination + * a b x y)
     (+ (* a x) (* b y))))

(define symbolic-call
  '(linear-combination (lambda (u v) (list 'add u v))
                       (lambda (u v) (list 'mul u v))
                       1 2 3 4))

(check (compile-and-run linear-combination symbolic-call)
       => '(add (mul 1 3) (mul 2 4)))
(check (fluid-let ((open-coded? open-coded-in-any-scope?))
         (compile-and-run linear-combination symbolic-call))
       => 11)

(check (compile-and-run '((lambda (+ a b) (+ a b)) * 3 4)) => 12)
(check (compile-and-run '((lambda (x) (+ x 1)) 2)) => 3)
(check (compile-and-run '(define (f)
                           (define (+ a b) (* a b))
                           (+ 3 4))
                        '(f))
       => 12)

(define (compiled exp)
  (statements (compile exp 'val 'next)))

(check (and (member '(assign val (op +) (reg arg1) (reg arg2))
                    (compiled '(lambda (x) (+ x 1))))
            #t)
       => #t)
(check (member '(assign val (op +) (reg arg1) (reg arg2))
               (compiled '(lambda (+) (+ 1 2))))
       => #f)
