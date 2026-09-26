(load "lib/check.scm")

;; (define acc (make-account 50)) creates E1, enclosed by the global
;; environment, binding balance = 50 and the internal procedures withdraw,
;; deposit and dispatch, all of whose environments are E1.  acc is bound to
;; dispatch.
;;
;; ((acc 'deposit) 40) creates a frame binding m = deposit, enclosed by E1,
;; which returns deposit; applying it creates a frame with amount = 40, also
;; enclosed by E1, where set! updates balance in E1 to 90.
;; ((acc 'withdraw) 60) goes the same way and leaves balance = 30 in E1.
;; The frames for the calls are discarded afterwards.
;;
;; The local state of acc lives in E1.  (define acc2 (make-account 100))
;; creates a distinct frame E2 with its own balance and its own internal
;; procedures, so the accounts are independent.  acc and acc2 share only the
;; global environment and the code of the procedure bodies.

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 50))
(check ((acc 'deposit) 40) => 90)
(check ((acc 'withdraw) 60) => 30)

(define acc2 (make-account 100))
(check ((acc2 'withdraw) 10) => 90)
(check ((acc 'deposit) 0) => 30)
(check (eq? (acc 'deposit) (acc2 'deposit)) => #f)
(check (eq? (acc 'deposit) (acc 'deposit)) => #t)
