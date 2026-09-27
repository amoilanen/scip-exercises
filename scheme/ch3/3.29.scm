(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/circuits.scm" (current-load-pathname)))

;; a1 or a2 = not ((not a1) and (not a2)).  A change of an input passes
;; through an inverter, the and-gate and the output inverter, so the delay is
;; 2 * inverter-delay + and-gate-delay.
(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (neither (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 neither)
    (inverter neither output)
    'ok))

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
(check (last (out-changes))
       => (list (+ start (* 2 inverter-delay) and-gate-delay) 0))
