(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/constraints.scm" (current-load-pathname)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv value)
  (let ((z (make-connector)))
    (constant value z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define c (make-connector))
(define f (celsius-fahrenheit-converter c))

(set-value! c 25 'user)
(check (get-value f) => 77)

(forget-value! c 'user)
(check (has-value? f) => #f)
(set-value! f 212 'user)
(check (get-value c) => 100)

(define x (make-connector))
(define y (make-connector))
(define difference (c- x y))
(define ratio (c/ x y))
(set-value! difference 6 'user)
(set-value! ratio 4 'user)
(check (has-value? x) => #f)
(set-value! y 2 'user)
(check (map get-value (list x difference ratio)) => '(8 6 4))
(check-error (set-value! x 9 'user))
