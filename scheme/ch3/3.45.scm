(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; serialized-exchange acquires the serializers of both accounts and then
;; calls the accounts' withdraw, which is now serialized with the same
;; serializer.  Its mutex is already held, by the exchange itself, and is
;; only released when the exchange returns, so the withdrawal waits
;; forever: every serialized exchange deadlocks, even with no other
;; process running.

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
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

(define a (make-account-and-serializer 10))
(define b (make-account-and-serializer 20))
(check ((a 'deposit) 20) => 30)
(check ((b 'withdraw) 5) => 15)
(check-error (serialized-exchange a b))

(check (possible-outcomes
        (lambda ()
          (let ((a (make-account-and-serializer 10))
                (b (make-account-and-serializer 20)))
            (parallel-execute (lambda () (serialized-exchange a b)))
            (list (a 'balance) (b 'balance)))))
       => '(deadlock))
