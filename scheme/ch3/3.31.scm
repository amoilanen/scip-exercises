(load "lib/check.scm")
(load "ch3/lib/circuits.scm")
(load "ch3/3.28.scm")

;; A gate computes its output only when one of its actions runs.  Running
;; each action as it is added makes every gate compute an output from the
;; wires' initial signals.  Without that, an output stays at 0 until some
;; input changes: an inverter on a 0 input gives 0, not 1.  In the
;; half-adder the inverted carry E stays 0, so setting one input to 1 never
;; raises the sum; and since the probes are actions too, they print nothing
;; until their wire changes.

(define (make-wire-without-initial-call)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; Runs the book's half-adder example and returns the probe output together
;; with the final sum and carry.
(define (half-adder-example)
  (fluid-let ((the-agenda (make-agenda)))
    (let ((input-1 (make-wire))
          (input-2 (make-wire))
          (sum (make-wire))
          (carry (make-wire)))
      (let ((output
             (with-output-to-string
               (lambda ()
                 (probe 'sum sum)
                 (probe 'carry carry)
                 (half-adder input-1 input-2 sum carry)
                 (set-signal! input-1 1)
                 (propagate)))))
        (list output (get-signal sum) (get-signal carry))))))

(check (half-adder-example)
       => (list (string-append "\nsum 0  New-value = 0"
                               "\ncarry 0  New-value = 0"
                               "\nsum 8  New-value = 1")
                1
                0))

(check (fluid-let ((make-wire make-wire-without-initial-call))
         (half-adder-example))
       => (list "" 0 0))

(define (inverter-output)
  (fluid-let ((the-agenda (make-agenda)))
    (let ((in (make-wire)) (out (make-wire)))
      (inverter in out)
      (propagate)
      (get-signal out))))

(check (inverter-output) => 1)
(check (fluid-let ((make-wire make-wire-without-initial-call))
         (inverter-output))
       => 0)
