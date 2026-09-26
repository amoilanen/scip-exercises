(load "lib/check.scm")
(load "ch3/lib/constraints.scm")

;; The multiplier only deduces a value once two of its three connectors
;; have one.  Given a, both factors are known and b = a * a follows.  Given
;; only b, it sees one known connector, the product, and does nothing: a
;; square root is never taken.

(define (squarer a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))
(squarer a b)

(set-value! a 3 'user)
(check (get-value b) => 9)

(forget-value! a 'user)
(check (has-value? b) => #f)
(set-value! b 16 'user)
(check (has-value? a) => #f)
