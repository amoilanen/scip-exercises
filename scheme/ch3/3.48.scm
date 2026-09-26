(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; A deadlock needs a cycle of processes, each waiting for an account held
;; by the next.  When every process acquires the accounts it needs in
;; increasing order of their numbers, a process only ever waits for an
;; account numbered higher than all the accounts it holds.  Going around a
;; cycle, the numbers would have to increase forever, so there is no cycle.
;; In the exchange problem, both exchanges of a and b now try a first; the
;; one that gets it also gets b while the other waits.

(define make-account-number
  (let ((last-number 0))
    (lambda ()
      (set! last-number (+ last-number 1))
      last-number)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (let ((current (atomic balance)))
      (atomic (set! balance (- current amount)))))
  (define (deposit amount)
    (let ((current (atomic balance)))
      (atomic (set! balance (+ current amount)))))
  (let ((number (make-account-number))
        (balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) (atomic balance))
            ((eq? m 'number) number)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange)) account1 account2)))

(define (ordered-serialized-exchange account1 account2)
  (let* ((in-order? (< (account1 'number) (account2 'number)))
         (lower (if in-order? account1 account2))
         (higher (if in-order? account2 account1)))
    (((lower 'serializer) ((higher 'serializer) exchange))
     account1
     account2)))

(define (opposite-exchanges serialized-exchange)
  (possible-outcomes
   (lambda ()
     (let ((a (make-account-and-serializer 10))
           (b (make-account-and-serializer 20)))
       (parallel-execute (lambda () (serialized-exchange a b))
                         (lambda () (serialized-exchange b a)))
       (list (a 'balance) (b 'balance))))))

(check (opposite-exchanges serialized-exchange)
       (=> same-set?)
       '((10 20) deadlock))
(check (opposite-exchanges ordered-serialized-exchange) => '((10 20)))

(define a (make-account-and-serializer 10))
(define b (make-account-and-serializer 20))
(check (< (a 'number) (b 'number)) => #t)
(ordered-serialized-exchange b a)
(check (list (a 'balance) (b 'balance)) => '(20 10))
