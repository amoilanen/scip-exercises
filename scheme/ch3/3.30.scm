(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/circuits.scm" (current-load-pathname)))
(load (merge-pathnames "3.28.scm" (current-load-pathname)))

;; The wire lists run from the most significant bit A1 to the least
;; significant An; the carry into An is a fresh wire, which stays 0.
(define (ripple-carry-adder as bs ss c)
  (if (not (= (length as) (length bs) (length ss)))
      (error "Wire lists differ in length -- RIPPLE-CARRY-ADDER" as bs ss))
  (let loop ((as as) (bs bs) (ss ss) (c-out c))
    (if (pair? as)
        (let ((c-in (make-wire)))
          (full-adder (car as) (car bs) c-in (car ss) c-out)
          (loop (cdr as) (cdr bs) (cdr ss) c-in))))
  'ok)

;; A half-adder's sum passes an or-gate (or an and-gate and an inverter)
;; and then an and-gate; its carry passes one and-gate.  A full adder's
;; carry out waits for a half-adder sum, a half-adder carry and an or-gate,
;; its sum for two half-adder sums.  The carry ripples through n - 1 adders
;; before the most significant one can finish:
;;
;;   Dha-s = max(Dor, Dand + Dinv) + Dand      Dha-c = Dand
;;   Dfa-c = Dha-s + Dha-c + Dor               Dfa-s = 2 Dha-s
;;   D(n)  = (n - 1) Dfa-c + max(Dfa-c, Dfa-s)
(define half-adder-sum-delay
  (+ (max or-gate-delay (+ and-gate-delay inverter-delay)) and-gate-delay))
(define half-adder-carry-delay and-gate-delay)
(define full-adder-carry-delay
  (+ half-adder-sum-delay half-adder-carry-delay or-gate-delay))
(define full-adder-sum-delay (* 2 half-adder-sum-delay))

(define (ripple-carry-delay n)
  (+ (* (- n 1) full-adder-carry-delay)
     (max full-adder-carry-delay full-adder-sum-delay)))

(define (make-wires n)
  (if (= n 0) '() (cons (make-wire) (make-wires (- n 1)))))

(define (number->bits number width)
  (let loop ((number number) (width width) (bits '()))
    (if (= width 0)
        bits
        (loop (quotient number 2)
              (- width 1)
              (cons (remainder number 2) bits)))))

(define (bits->number bits)
  (fold-left (lambda (number bit) (+ (* 2 number) bit)) 0 bits))

(define width 4)
(define as (make-wires width))
(define bs (make-wires width))
(define ss (make-wires width))
(define c (make-wire))
(ripple-carry-adder as bs ss c)

(define (add x y)
  (for-each set-signal! as (number->bits x width))
  (for-each set-signal! bs (number->bits y width))
  (propagate)
  (+ (bits->number (map get-signal ss))
     (* (get-signal c) (expt 2 width))))

(check (add 0 0) => 0)
(check (add 5 3) => 8)
(check (add 9 6) => 15)
(check (add 15 1) => 16)
(check (add 15 15) => 30)
(check (add 6 0) => 6)

;; 15 + 1 makes the carry ripple through every stage: the worst case.
(check (ripple-carry-delay 4) => 64)
(add 0 0)
(define c-changes (record-signal c))
(define start (current-time the-agenda))
(check (add 15 1) => 16)
(check (- (current-time the-agenda) start) => (ripple-carry-delay 4))
(check (c-changes)
       => (list (list start 0) (list (+ start (ripple-carry-delay 4)) 1)))

(check-error (ripple-carry-adder as bs (cdr ss) c))
