;; A minimal test library in the spirit of SRFI 78.
;;
;;   (check expr => expected)          compares with equal?
;;   (check expr (=> same?) expected)  compares with a custom predicate
;;   (check-error expr)                expects evaluation of expr to signal an error
;;
;; A failing check signals an error, so running a file whose checks fail
;; terminates MIT Scheme with a non-zero exit code.

(define (run-check expression thunk same? expected)
  (let ((actual (thunk)))
    (if (not (same? actual expected))
        (error "Check failed:" expression 'expected expected 'actual actual))))

(define-syntax check
  (syntax-rules (=>)
    ((_ expr (=> same?) expected)
     (run-check 'expr (lambda () expr) same? expected))
    ((_ expr => expected)
     (run-check 'expr (lambda () expr) equal? expected))))

(define-syntax check-error
  (syntax-rules ()
    ((_ expr)
     (if (not (call-with-current-continuation
               (lambda (k)
                 (with-exception-handler
                  (lambda (e) (k #t))
                  (lambda () expr #f)))))
         (error "Check failed, expected an error:" 'expr)))))

(define (approx= tolerance)
  (lambda (actual expected)
    (< (abs (- actual expected)) tolerance)))
