(load "lib/check.scm")
(load "ch3/lib/circuits.scm")

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1)) 1 0))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(check (map logical-or '(0 0 1 1) '(0 1 0 1)) => '(0 1 1 1))

(define a (make-wire))
(define b (make-wire))
(define out (make-wire))
(or-gate a b out)
(define out-changes (record-signal out))

(define (settle a-value b-value)
  (set-signal! a a-value)
  (set-signal! b b-value)
  (propagate)
  (get-signal out))

(check (map settle '(0 0 1 1) '(0 1 0 1)) => '(0 1 1 1))

(define start (current-time the-agenda))
(check (settle 0 0) => 0)
(check (last (out-changes)) => (list (+ start or-gate-delay) 0))
