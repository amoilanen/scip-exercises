(load "lib/check.scm")
(load "ch3/lib/streams.scm")

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (louis-pairs (stream-cdr s) (stream-cdr t))))

;; It does not work: interleave is an ordinary procedure, so its second
;; argument (louis-pairs (stream-cdr s) (stream-cdr t)) is evaluated before
;; interleave is called.  Without a cons-stream to delay it, each call
;; immediately makes the next one, forcing s and t ever further, and
;; (louis-pairs integers integers) never returns.

;; The integers from n, signalling an error once forced past limit.
(define (integers-up-to-tripwire n limit)
  (if (> n limit)
      (error "Stream forced too far:" n)
      (cons-stream n (integers-up-to-tripwire (+ n 1) limit))))

(define (guarded-integers) (integers-up-to-tripwire 1 50))

(check (stream-head (pairs (guarded-integers) (guarded-integers)) 5)
       => '((1 1) (1 2) (2 2) (1 3) (2 3)))
(check-error (louis-pairs (guarded-integers) (guarded-integers)))
