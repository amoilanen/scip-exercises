(load "lib/check.scm")
(load "ch3/lib/circuits.scm")

;; When a wire changes, each gate reading it computes its new output at once
;; and schedules setting it after the gate's delay.  If an and-gate's inputs
;; go from 0,1 to 1,0 in the same segment, the first change schedules "set
;; to 1" and the second "set to 0".  Run in the order they were scheduled,
;; the output ends at 0, the value for the latest inputs.  Run last in,
;; first out, "set to 0" comes first and the stale "set to 1" wins, leaving
;; the output at 1 although the inputs are 1,0.

(define (make-stack) (list 'stack))
(define (empty-stack? stack) (null? (cdr stack)))
(define (top stack) (cadr stack))
(define (push! stack item) (set-cdr! stack (cons item (cdr stack))))
(define (pop! stack) (set-cdr! stack (cddr stack)))

;; Returns the (time value) changes of the and-gate's output.
(define (and-gate-inputs-swap)
  (fluid-let ((the-agenda (make-agenda)))
    (let ((a1 (make-wire)) (a2 (make-wire)) (output (make-wire)))
      (and-gate a1 a2 output)
      (set-signal! a2 1)
      (propagate)
      (let ((changes (record-signal output)))
        (set-signal! a1 1)
        (set-signal! a2 0)
        (propagate)
        (changes)))))

;; The output settles at 0 one and-gate delay in, when the inputs change;
;; it reacts one delay later.
(define settled and-gate-delay)
(define reacted (* 2 and-gate-delay))

(check (and-gate-inputs-swap)
       => (list (list settled 0) (list reacted 1) (list reacted 0)))

(check (fluid-let ((make-queue make-stack)
                   (empty-queue? empty-stack?)
                   (front-queue top)
                   (insert-queue! push!)
                   (delete-queue! pop!))
         (and-gate-inputs-swap))
       => (list (list settled 0) (list reacted 1)))
