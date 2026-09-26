;; The digital circuit simulator of section 3.3.4.
;; half-adder and full-adder use or-gate, which is exercise 3.28 (or 3.29):
;; load a file that defines it before building adders.

;;; Queues (section 3.3.2)

(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (list item)))
    (if (empty-queue? queue)
        (set-front-ptr! queue new-pair)
        (set-cdr! (rear-ptr queue) new-pair))
    (set-rear-ptr! queue new-pair)
    queue))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE! called with an empty queue" queue)
      (begin (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))

;;; Wires

(define (call-each procedures)
  (for-each (lambda (proc) (proc)) procedures))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action) ((wire 'add-action!) action))

;;; The agenda: the current time followed by time segments in increasing
;;; order of time, each holding a queue of the actions due at that time.

(define (make-time-segment time queue) (cons time queue))
(define (segment-time segment) (car segment))
(define (segment-queue segment) (cdr segment))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (new-segment)
    (let ((queue (make-queue)))
      (insert-queue! queue action)
      (make-time-segment time queue)))
  (define (add-to-segments! segments)
    (let ((next (cdr segments)))
      (cond ((or (null? next) (< time (segment-time (car next))))
             (set-cdr! segments (cons (new-segment) next)))
            ((= time (segment-time (car next)))
             (insert-queue! (segment-queue (car next)) action))
            (else (add-to-segments! next)))))
  (if (or (empty-agenda? agenda)
          (< time (segment-time (first-segment agenda))))
      (set-segments! agenda (cons (new-segment) (segments agenda)))
      (add-to-segments! agenda)))

(define (remove-first-agenda-item! agenda)
  (let ((queue (segment-queue (first-segment agenda))))
    (delete-queue! queue)
    (if (empty-queue? queue)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((segment (first-segment agenda)))
        (set-current-time! agenda (segment-time segment))
        (front-queue (segment-queue segment)))))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;;; Gates and circuits

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1)) 1 0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

;; Like probe, but records each (time value) change of the wire; calling the
;; returned procedure gives the changes so far, oldest first.
(define (record-signal wire)
  (let ((changes '()))
    (add-action! wire
                 (lambda ()
                   (set! changes
                         (cons (list (current-time the-agenda)
                                     (get-signal wire))
                               changes))))
    (lambda () (reverse changes))))
