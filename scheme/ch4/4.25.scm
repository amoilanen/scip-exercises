(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/lazy.scm" (current-load-pathname)))

;; In applicative-order Scheme (factorial 5) never returns: the operand
;; (* n (factorial (- n 1))) is evaluated before unless is applied, so every
;; call recurses again, also for n = 1, and the recursion never stops.
;; In a normal-order language the definition works and gives 120.

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

;; The guard turns the endless recursion into an error.
(define (factorial n)
  (if (< n -10)
      (error "Runaway recursion -- FACTORIAL" n))
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(check-error (factorial 5))

(check (interpret '(define (unless condition usual-value exceptional-value)
                     (if condition exceptional-value usual-value))
                  '(define (factorial n)
                     (unless (= n 1)
                             (* n (factorial (- n 1)))
                             1))
                  '(factorial 5))
       => 120)
