(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; Sequentially, each exchange swaps two balances, so any number of them
;; only permutes 10, 20 and 30.  serialized-exchange holds the serializers
;; of both its accounts throughout, so exchanges sharing an account run one
;; after the other and the balances still end as a permutation.
;;
;; Louis's unserialized exchange of a and b can read a = 10, b = 20 (and
;; compute -10); an exchange of b and c then runs completely, making b = 30,
;; c = 20; now the first withdraws -10 from a and deposits -10 into b,
;; leaving 20, 20, 20.  The sum is still 60: every exchange withdraws some
;; amount from one account and deposits the same amount into another, and
;; each withdrawal and deposit is serialized, so none is lost.
;;
;; Without serializing the transactions on each account, the withdrawal from
;; b by one exchange and the deposit into b by the other can both read b
;; before either writes it; one update is lost and the sum changes.

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (let ((current (atomic balance)))
      (atomic (set! balance (- current amount)))))
  (define (deposit amount)
    (let ((current (atomic balance)))
      (atomic (set! balance (+ current amount)))))
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) (atomic balance))
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

;; The account of section 3.4.2, whose withdrawals and deposits are
;; serialized.  To other processes each is then a single indivisible step,
;; and it is modelled as one to keep the number of orders small.
(define (make-account balance)
  (define (withdraw amount) (atomic (set! balance (- balance amount))))
  (define (deposit amount) (atomic (set! balance (+ balance amount))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) (atomic balance))
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange)) account1 account2)))

(define (final-balances make-account exchange)
  (possible-outcomes
   (lambda ()
     (let ((a (make-account 10))
           (b (make-account 20))
           (c (make-account 30)))
       (parallel-execute (lambda () (exchange a b))
                         (lambda () (exchange b c)))
       (map (lambda (account) (account 'balance)) (list a b c))))))

(define (total balances) (apply + balances))

(define (permutation? balances)
  (equal? (sort balances <) '(10 20 30)))

(check (final-balances make-account-and-serializer serialized-exchange)
       (=> same-set?)
       '((20 30 10) (30 10 20)))

(define louis-balances (final-balances make-account exchange))
(check (map total louis-balances) => '(60 60 60))
(check (remove permutation? louis-balances) => '((20 20 20)))

(define unprotected-balances
  (final-balances make-account-and-serializer exchange))
(check (remove (lambda (balances) (= (total balances) 60))
               unprotected-balances)
       (=> same-set?)
       '((20 30 20) (20 10 20)))
