(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "4.16.scm" (current-load-pathname)))

;; Ben (sequential definitions) gets 16, Alyssa (scanned out, 4.16) gets an
;; error, Eva (truly simultaneous definitions) gets 20. Eva's view matches
;; the idea of simultaneous scope, but Alyssa's is the practical choice, and
;; the one MIT Scheme makes: an error is better than a surprising answer, and
;; no strategy can be simultaneous when definitions depend on each other
;; circularly.
;;
;; Eva's behaviour can be had by evaluating each internal definition on
;; demand: all names are first bound to deferred value expressions, and the
;; first lookup of a name evaluates its expression and stores the value.
;; The defines are then forced in their original order, so their side
;; effects happen no later than in sequential order. While a value is being
;; computed its name is *unassigned*, so a circular dependency is still an
;; error.

(define puzzle
  '(let ((a 1))
     (define (f x)
       (define b (+ a x))
       (define a 5)
       (+ a b))
     (f 10)))

(check (fluid-let ((scan-out-defines (lambda (body) body)))
         (interpret puzzle))
       => 16)
(check-error (interpret puzzle))

(define-record-type deferred-definition
  (make-deferred-definition exp env)
  deferred-definition?
  (exp deferred-definition-exp)
  (env deferred-definition-env))

(define (deferred-definition-form? exp) (tagged-list? exp 'define-deferred))

(define (eval-deferred-definition exp env)
  (define-variable! (definition-variable exp)
                    (make-deferred-definition (definition-value exp) env)
                    env)
  'ok)

(define (force-definition! var definition env)
  (set-variable-value! var '*unassigned* env)
  (let ((value (mc-eval (deferred-definition-exp definition)
                        (deferred-definition-env definition))))
    (set-variable-value! var value env)
    value))

(define lookup-assigned-value lookup-variable-value)

(define (lookup-variable-value var env)
  (let ((value (lookup-assigned-value var env)))
    (if (deferred-definition? value)
        (force-definition! var value env)
        value)))

(define eval-without-deferred-definitions mc-eval)

(define (mc-eval exp env)
  (if (deferred-definition-form? exp)
      (eval-deferred-definition exp env)
      (eval-without-deferred-definitions exp env)))

(define (scan-out-defines body)
  (append (map (lambda (definition)
                 (list 'define-deferred
                       (definition-variable definition)
                       (definition-value definition)))
               (internal-definitions body))
          (map (lambda (exp)
                 (if (definition? exp)
                     (definition-variable exp)
                     exp))
               body)))

(check (scan-out-defines '((define b (+ a 1)) (define (g) b) (g)))
       => '((define-deferred b (+ a 1))
            (define-deferred g (lambda () b))
            b
            g
            (g)))
(check (interpret puzzle) => 20)
(check (interpret '(define (f x)
                     (define (even? n) (if (= n 0) true (odd? (- n 1))))
                     (define (odd? n) (if (= n 0) false (even? (- n 1))))
                     (even? x))
                  '(f 9))
       => #f)
(check (interpret '(define order '())
                  '(define (note x) (set! order (cons x order)) x)
                  '(define (f)
                     (define a (note 'a))
                     (define b (note 'b))
                     (note 'body))
                  '(f)
                  'order)
       => '(body b a))
(check-error (interpret '(define (f)
                           (define a b)
                           (define b a)
                           a)
                        '(f)))
