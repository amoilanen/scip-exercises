(load "lib/check.scm")
(load "ch4/4.6.scm")

(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

;; (let name ((v e) ...) body) becomes
;;   (((lambda () (define name (lambda (v ...) body)) name)) e ...)
;; so the inits are evaluated outside the scope of name.
(define (named-let->combination exp)
  (let ((name (named-let-name exp))
        (bindings (named-let-bindings exp)))
    (let ((procedure (make-lambda (map binding-variable bindings)
                                  (named-let-body exp))))
      (cons (list (make-lambda '() (list (list 'define name procedure) name)))
            (map binding-value bindings)))))

(define ordinary-let->combination let->combination)

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
      (ordinary-let->combination exp)))

(define (fib n)
  (interpret '(define (fib n)
                (let fib-iter ((a 1) (b 0) (count n))
                  (if (= count 0)
                      b
                      (fib-iter (+ a b) a (- count 1)))))
             (list 'fib n)))

(check (fib 0) => 0)
(check (fib 1) => 1)
(check (fib 10) => 55)
(check (interpret '(let ((x 1) (y 2)) (+ x y))) => 3)
(check (interpret '(define loop 3)
                  '(let loop ((i loop) (acc '()))
                     (if (= i 0)
                         acc
                         (loop (- i 1) (cons i acc)))))
       => '(1 2 3))
