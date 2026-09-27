(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch given-password m)
    (cond ((not (eq? given-password password))
           (lambda (amount) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define (valid-password? account password)
  (number? ((account password 'deposit) 0)))

(define (make-joint account password new-password)
  (if (not (valid-password? account password))
      (error "Incorrect password -- MAKE-JOINT" password))
  (lambda (given-password m)
    (if (eq? given-password new-password)
        (account password m)
        (lambda (amount) "Incorrect password"))))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

(check ((paul-acc 'rosebud 'withdraw) 30) => 70)
(check ((peter-acc 'open-sesame 'deposit) 10) => 80)
(check ((paul-acc 'rosebud 'deposit) 0) => 80)

(check ((paul-acc 'open-sesame 'withdraw) 10) => "Incorrect password")
(check ((peter-acc 'rosebud 'withdraw) 10) => "Incorrect password")

(define mary-acc (make-joint paul-acc 'rosebud 'swordfish))
(check ((mary-acc 'swordfish 'withdraw) 50) => 30)

(check-error (make-joint peter-acc 'wrong 'new))
