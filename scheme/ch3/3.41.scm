(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; Ben's concern is unfounded.  Reading the balance is a single step and
;; withdraw and deposit each change it with a single write, so an
;; unserialized read already sees the balance either before or after each
;; transaction, never anything in between; serializing the read excludes
;; nothing.  It would matter only if a transaction left an inconsistent
;; balance between steps, e.g. by writing it twice.

(define (make-account balance serialize-balance?)
  (define (withdraw amount)
    (let ((current (atomic balance)))
      (if (>= current amount)
          (begin (atomic (set! balance (- current amount)))
                 (- current amount))
          "Insufficient funds")))
  (define (deposit amount)
    (let ((current (atomic balance)))
      (atomic (set! balance (+ current amount)))
      (+ current amount)))
  (define (read-balance) (atomic balance))
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             (if serialize-balance?
                 ((protected read-balance))
                 (read-balance)))
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (observed-balances serialize-balance?)
  (possible-outcomes
   (lambda ()
     (let ((acc (make-account 100 serialize-balance?))
           (seen #f))
       (parallel-execute (lambda () ((acc 'withdraw) 10))
                         (lambda () ((acc 'deposit) 25))
                         (lambda () (set! seen (acc 'balance))))
       seen))))

(check (observed-balances #f) (=> same-set?) '(90 100 115 125))
(check (observed-balances #t) (=> same-set?) '(90 100 115 125))
