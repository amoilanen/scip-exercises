(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; Ordering fails when a process must already hold a shared resource to
;; find out which other resources it needs.  Say each account names a
;; beneficiary, which may be changed at any time, and paying it must read
;; the beneficiary while holding the account and then acquire the
;; beneficiary's account too.  If a names b and b names a, one payment
;; holds a and needs b while the other holds b and needs a.  Whichever of
;; them waits for a lower-numbered account learns so only once it holds
;; the higher-numbered one, and releasing it would let the beneficiary
;; change.

(define (make-account balance)
  (let ((beneficiary #f)
        (balance-serializer (make-serializer)))
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
            ((eq? m 'beneficiary) (atomic beneficiary))
            ((eq? m 'set-beneficiary!)
             (lambda (account) (atomic (set! beneficiary account))))
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (pay-beneficiary account amount)
  (define (pay)
    (let ((beneficiary (account 'beneficiary)))
      (define (transfer)
        ((account 'withdraw) amount)
        ((beneficiary 'deposit) amount))
      (((beneficiary 'serializer) transfer))))
  (((account 'serializer) pay)))

(define (make-accounts-naming-each-other)
  (let ((a (make-account 10))
        (b (make-account 20)))
    ((a 'set-beneficiary!) b)
    ((b 'set-beneficiary!) a)
    (list a b)))

(define (balances accounts)
  (map (lambda (account) (account 'balance)) accounts))

(let ((accounts (make-accounts-naming-each-other)))
  (pay-beneficiary (car accounts) 3)
  (check (balances accounts) => '(7 23)))

(check (possible-outcomes
        (lambda ()
          (let ((accounts (make-accounts-naming-each-other)))
            (parallel-execute
             (lambda () (pay-beneficiary (car accounts) 3))
             (lambda () (pay-beneficiary (cadr accounts) 5)))
            (balances accounts))))
       (=> same-set?)
       '((12 18) deadlock))
