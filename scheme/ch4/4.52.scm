(load "lib/check.scm")
(load "ch4/lib/amb.scm")

(define (if-fail-expression exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((expression-proc (analyze (if-fail-expression exp)))
        (alternative-proc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (expression-proc env
                       succeed
                       (lambda ()
                         (alternative-proc env succeed fail))))))

(install-special-form! 'if-fail analyze-if-fail)

(define env (amb-environment))

(define (first-even items)
  `(if-fail (let ((x (an-element-of ',items)))
              (require (even? x))
              x)
            'all-odd))

(check (amb-collect (first-even '(1 3 5)) env 1) => '(all-odd))
(check (amb-collect (first-even '(1 3 5 8)) env 1) => '(8))
(check (amb-collect (first-even '()) env 1) => '(all-odd))

(check (amb-collect '(if-fail (amb 1 2) (car '())) env 2) => '(1 2))
(check (amb-collect '(if-fail (amb) (amb)) env) => '())
