(load "lib/check.scm")
(load "ch4/lib/mceval.scm")

(define (make-lexical-address frame-number displacement)
  (list frame-number displacement))
(define (lexical-address-frame address) (car address))
(define (lexical-address-displacement address) (cadr address))

;; The list of values in the frame, starting at the variable's value.
(define (lexical-address-values address env)
  (define (frame-at env n)
    (if (= n 0)
        (first-frame env)
        (frame-at (enclosing-environment env) (- n 1))))
  (list-tail (frame-values (frame-at env (lexical-address-frame address)))
             (lexical-address-displacement address)))

(define (lexical-address-lookup address env)
  (let ((value (car (lexical-address-values address env))))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable at lexical address" address)
        value)))

(define (lexical-address-set! address value env)
  (set-car! (lexical-address-values address env) value))

(define env
  (extend-environment '(y z)
                      '(1 2)
                      (extend-environment '(a b c d e)
                                          '(3 4 5 6 *unassigned*)
                                          (extend-environment '(x y)
                                                              '(7 8)
                                                              '()))))

(check (lexical-address-lookup (make-lexical-address 0 0) env) => 1)
(check (lexical-address-lookup (make-lexical-address 0 1) env) => 2)
(check (lexical-address-lookup (make-lexical-address 1 2) env) => 5)
(check (lexical-address-lookup (make-lexical-address 2 1) env) => 8)
(check-error (lexical-address-lookup (make-lexical-address 1 4) env))

(lexical-address-set! (make-lexical-address 1 4) 9 env)
(check (lexical-address-lookup (make-lexical-address 1 4) env) => 9)
(lexical-address-set! (make-lexical-address 2 1) 10 env)
(check (lexical-address-lookup (make-lexical-address 2 1) env) => 10)
(check (lexical-address-lookup (make-lexical-address 0 0) env) => 1)
