(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/serializers.scm" (current-load-pathname)))

;; The change is safe and allows exactly the same concurrency.  A
;; serialized procedure acquires the serializer's mutex around each call,
;; so concurrent calls of the one protected-withdraw exclude each other just
;; as calls of freshly serialized procedures do: what matters is the shared
;; serializer, not which serialized procedure is called.

(define (make-account-with serialize-per-call?)
  (lambda (balance)
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
    (let* ((protected (make-serializer))
           (protected-withdraw (protected withdraw))
           (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw)
               (if serialize-per-call?
                   (protected withdraw)
                   protected-withdraw))
              ((eq? m 'deposit)
               (if serialize-per-call?
                   (protected deposit)
                   protected-deposit))
              ((eq? m 'balance) (atomic balance))
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
      dispatch)))

(define make-account (make-account-with #t))
(define make-bens-account (make-account-with #f))

;; Each outcome lists the final balance and the value each call returned.
(define (outcomes make-account)
  (possible-outcomes
   (lambda ()
     (let ((acc (make-account 100))
           (results (make-vector 3)))
       (parallel-execute
        (lambda () (vector-set! results 0 ((acc 'withdraw) 10)))
        (lambda () (vector-set! results 1 ((acc 'withdraw) 20)))
        (lambda () (vector-set! results 2 ((acc 'deposit) 5))))
       (cons (acc 'balance) (vector->list results))))))

(check (length (outcomes make-account)) => 6)
(check (delete-duplicates (map car (outcomes make-account))) => '(75))
(check (outcomes make-bens-account) (=> same-set?) (outcomes make-account))
