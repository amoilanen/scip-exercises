(load "lib/check.scm")
(load "ch4/4.6.scm")

;;; a.

(define find-variable-value lookup-variable-value)

(define (lookup-variable-value var env)
  (let ((value (find-variable-value var env)))
    (if (eq? value '*unassigned*)
        (error "Unassigned variable" var)
        value)))

;;; b.

(define (internal-definitions body)
  (filter definition? body))

(define (definition->assignment exp)
  (list 'set! (definition-variable exp) (definition-value exp)))

(define (scan-out-defines body)
  (let ((definitions (internal-definitions body)))
    (if (null? definitions)
        body
        (list (make-let
               (map (lambda (definition)
                      (list (definition-variable definition)
                            ''*unassigned*))
                    definitions)
               (map (lambda (exp)
                      (if (definition? exp)
                          (definition->assignment exp)
                          exp))
                    body))))))

;;; c. In make-procedure: it runs once each time a lambda expression is
;;; evaluated, whereas procedure-body runs on every application of the
;;; procedure, which would repeat the transformation for each call.

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(check (scan-out-defines '((define u 1) (display u) (define (v) u) (v)))
       => '((let ((u '*unassigned*) (v '*unassigned*))
              (set! u 1)
              (display u)
              (set! v (lambda () u))
              (v))))
(check (scan-out-defines '((+ 1 2))) => '((+ 1 2)))

(check (interpret '(define (f x)
                     (define (even? n) (if (= n 0) true (odd? (- n 1))))
                     (define (odd? n) (if (= n 0) false (even? (- n 1))))
                     (list (even? x) (odd? x)))
                  '(f 7))
       => '(#f #t))
(check (interpret '(define (g)
                     (define a 1)
                     (define b (+ a 1))
                     (* a b))
                  '(g))
       => 2)

;; Without scanning out, b sees the global c. With it, the internal c
;; shadows the global one throughout the body and is still unassigned when b
;; is defined.
(define shadowing-program
  '((define c 10)
    (define (h)
      (define b c)
      (define c 2)
      b)
    (h)))

(check (fluid-let ((scan-out-defines (lambda (body) body)))
         (apply interpret shadowing-program))
       => 10)
(check-error (apply interpret shadowing-program))
