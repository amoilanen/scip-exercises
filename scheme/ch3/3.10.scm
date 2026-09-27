(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

;; (define W1 (make-withdraw 100)) applies make-withdraw, creating E1 with
;; initial-amount = 100.  The let is an application of a lambda, so it
;; creates E2 with balance = 100, enclosed by E1.  W1 is the inner lambda
;; with environment E2.
;;
;; (W1 50) creates a frame binding amount = 50, enclosed by E2; set! changes
;; balance in E2 to 50 while initial-amount in E1 stays 100.
;;
;; (define W2 (make-withdraw 100)) builds a separate pair of frames E3 and E4
;; in the same way; W1 and W2 share only the code.
;;
;; The only difference from the version without let is the extra frame
;; holding initial-amount, so both versions behave identically.

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw-without-let balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(define V1 (make-withdraw-without-let 100))

(check (W1 50) => 50)
(check (V1 50) => 50)
(check (W2 70) => 30)
(check (W1 60) => "Insufficient funds")
(check (V1 60) => "Insufficient funds")
(check (W1 50) => 0)
