(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

;; To check the hand simulations, the simulator records a snapshot of the
;; stack (top first) after every save and restore, showing labels by name.

(define label-names '())
(define stack-mirror '())
(define snapshots '())

(define (make-label-entry name insts)
  (set! label-names (cons (cons insts name) label-names))
  (cons name insts))

(define (value->shown value)
  (let ((label (assq value label-names)))
    (if label (cdr label) value)))

(define (record-snapshot! stack)
  (set! stack-mirror stack)
  (set! snapshots (cons stack snapshots)))

(define (push stack value)
  ((stack 'push) value)
  (record-snapshot! (cons (value->shown value) stack-mirror)))

(define (pop stack)
  (let ((value (stack 'pop)))
    (record-snapshot! (cdr stack-mirror))
    value))

(define (stack-history machine-maker n)
  (set! label-names '())
  (set! stack-mirror '())
  (set! snapshots '())
  (let ((result (run-machine (machine-maker) (list (list 'n n)) 'val)))
    (cons result (reverse snapshots))))

;; Factorial of 3. Each recursive call saves the caller's continue and n;
;; after the base case the frames are popped in reverse order.
(check (stack-history make-factorial-machine 3)
       => '(6
            (fact-done)
            (3 fact-done)
            (after-fact 3 fact-done)
            (2 after-fact 3 fact-done)
            (after-fact 3 fact-done)            ; n = 2 restored
            (3 fact-done)                       ; val = 2 * 1
            (fact-done)                         ; n = 3 restored
            ()))                                ; val = 3 * 2

;; Fib(3). Computing Fib(n - 1) saves continue and n; computing Fib(n - 2)
;; saves continue and Fib(n - 1) (in val).
(check (stack-history make-fib-machine 3)
       => '(2
            (fib-done)                          ; Fib(3) starts
            (3 fib-done)
            (afterfib-n-1 3 fib-done)           ; Fib(2) starts
            (2 afterfib-n-1 3 fib-done)
            (afterfib-n-1 3 fib-done)           ; Fib(1) = 1 returned
            (3 fib-done)
            (afterfib-n-1 3 fib-done)
            (1 afterfib-n-1 3 fib-done)         ; Fib(1) kept, Fib(0) starts
            (afterfib-n-1 3 fib-done)           ; Fib(0) = 0 returned
            (3 fib-done)                        ; Fib(2) = 1
            (fib-done)
            ()
            (fib-done)
            (1 fib-done)                        ; Fib(2) kept, Fib(1) starts
            (fib-done)
            ()))                                ; Fib(3) = 1 + 1
