(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "5.42.scm" (current-load-pathname)))

;; An internal definition adds a binding to the frame at run time, which the
;; compile-time environment doesn't know about, and it moves the other
;; bindings of the frame: define-variable! puts the new binding in front.

(define compile-lambda-body-with-definitions compile-lambda-body)

(define shadowed-parameter '((lambda (x) (define y 2) x) 1))

(check (compile-and-run shadowed-parameter) => 2)

;; The body is scanned out as in exercise 4.16, with the let written as a
;; combination because the compiler doesn't handle let:
;;
;;   ((lambda (u v) (set! u e1) (set! v e2) e3)
;;    '*unassigned* '*unassigned*)
(define (scan-out-defines body)
  (let ((variables (map definition-variable (filter definition? body))))
    (define (definition->assignment exp)
      (if (definition? exp)
          (list 'set! (definition-variable exp) (definition-value exp))
          exp))
    (if (null? variables)
        body
        (list (cons (make-lambda variables (map definition->assignment body))
                    (map (lambda (variable) ''*unassigned*) variables))))))

(define (compile-lambda-body exp proc-entry)
  (compile-lambda-body-with-definitions
   (make-lambda (lambda-parameters exp)
                (scan-out-defines (lambda-body exp)))
   proc-entry))

(check (scan-out-defines '((define (f) 1) (display 2) (define g 3) (f)))
       => '(((lambda (f g)
               (set! f (lambda () 1))
               (display 2)
               (set! g 3)
               (f))
             '*unassigned*
             '*unassigned*)))
(check (scan-out-defines '((+ 1 2))) => '((+ 1 2)))

(check (compile-and-run shadowed-parameter) => 1)

(define (performs-definition? code)
  (any (lambda (inst)
         (and (tagged-list? inst 'perform)
              (equal? (cadr inst) '(op define-variable!))))
       code))

(check (performs-definition?
        (statements (compile '(lambda (x) (define y 2) x) 'val 'next)))
       => #f)
(check (performs-definition? (statements (compile '(define y 2) 'val 'next)))
       => #t)

(check (compile-and-run
        '(define (parity n)
           (define (even? n) (if (= n 0) 'even (odd? (- n 1))))
           (define (odd? n) (if (= n 0) 'odd (even? (- n 1))))
           (even? n))
        '(list (parity 10) (parity 7)))
       => '(even odd))

(check (compile-and-run
        '(define (sum-of-squares a b)
           (define (square x) (* x x))
           (define result (+ (square a) (square b)))
           result)
        '(sum-of-squares 3 4))
       => 25)

(check-error (compile-and-run '((lambda () (define a b) (define b 1) a))))
