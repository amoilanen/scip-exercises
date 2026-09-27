(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/constraints.scm" (current-load-pathname)))

;; (define a (make-connector)) evaluates the body of make-connector in E1,
;; whose parent is the global environment.  E1 binds value, informant and
;; constraints, and the internal procedures set-my-value, forget-my-value,
;; connect and me; the global a is the procedure me, whose environment is
;; E1.  b is built the same way in its own E2.
;;
;; (set-value! a 10 'user) applies the global set-value! in E3 (parent
;; global: connector = a, new-value = 10, informant = user).  (a 'set-value!)
;; runs in E4 (parent E1: request = set-value!) and returns set-my-value,
;; which is applied in E5 (parent E1: newval = 10, setter = user).  a has no
;; value yet, so value and informant in E1 are set to 10 and user, and
;;
;;   (for-each-except setter inform-about-value constraints)
;;
;; is evaluated in E5: setter is found in E5, inform-about-value in the
;; global environment and constraints in E1.  The call makes E6, whose
;; parent is the global environment, where for-each-except was defined.  E6
;; binds exception = user, procedure = inform-about-value, list = () (a has
;; no constraints yet) and the internal procedure loop.  (loop list) runs in
;; E7 (parent E6: items = ()) and returns done.  Only E1 keeps a's new
;; state; E3 to E7 are garbage once set-value! returns.

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)
(check (get-value a) => 10)
(check (has-value? b) => #f)

(define informed '())
(for-each-except 'user
                 (lambda (item) (set! informed (cons item informed)))
                 '(adder user multiplier))
(check informed => '(multiplier adder))
