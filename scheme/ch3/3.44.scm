(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; Ben is right, as long as withdrawals and deposits are serialized per
;; account.  The exchange goes wrong because it computes the difference
;; from balances it read earlier, which another process may have changed
;; by the time it moves the money.  A transfer's amount does not depend on
;; any balance, so running its withdrawal and deposit at any time, each
;; atomically, gives the same result.

;; Serialized withdrawals and deposits, each one indivisible step.
(define (make-account balance)
  (define (withdraw amount) (atomic (set! balance (- balance amount))))
  (define (deposit amount) (atomic (set! balance (+ balance amount))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) (atomic balance))
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; Withdrawals and deposits that read and then write the balance.
(define (make-unserialized-account balance)
  (define (withdraw amount)
    (let ((current (atomic balance)))
      (atomic (set! balance (- current amount)))))
  (define (deposit amount)
    (let ((current (atomic balance)))
      (atomic (set! balance (+ current amount)))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) (atomic balance))
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

(define (final-balances make-account)
  (possible-outcomes
   (lambda ()
     (let ((a (make-account 10))
           (b (make-account 20))
           (c (make-account 30)))
       (parallel-execute (lambda () (transfer a b 5))
                         (lambda () (transfer b c 7)))
       (map (lambda (account) (account 'balance)) (list a b c))))))

(check (final-balances make-account) => '((5 18 37)))
(check (final-balances make-unserialized-account)
       (=> same-set?)
       '((5 18 37) (5 13 37) (5 25 37)))
