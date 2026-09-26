(load "lib/check.scm")
(load "ch4/4.16.scm")

;; The alternative evaluates every definition's value expression before any
;; of the names is assigned. In solve, (stream-map f y) is then evaluated
;; while y is still *unassigned*, and stream-map needs the stream-car of y
;; right away, so the procedure fails. With the strategy of 4.16, y is
;; assigned before dy's value expression is evaluated, so it works: only the
;; integral delays its use of dy.
;;
;; The checks use a smaller program with the same shape: the value of the
;; second definition needs the value of the first.

(define (scan-out-defines-simultaneously body)
  (let* ((definitions (internal-definitions body))
         (temporaries (map (lambda (definition)
                             (generate-uninterned-symbol))
                           definitions)))
    (if (null? definitions)
        body
        (list (make-let
               (map (lambda (definition)
                      (list (definition-variable definition)
                            ''*unassigned*))
                    definitions)
               (cons (make-let
                      (map (lambda (temporary definition)
                             (list temporary (definition-value definition)))
                           temporaries
                           definitions)
                      (map (lambda (temporary definition)
                             (list 'set!
                                   (definition-variable definition)
                                   temporary))
                           temporaries
                           definitions))
                     (remove definition? body)))))))

(define dependent-definitions
  '((define (f)
      (define y 1)
      (define dy (+ y 1))
      (list y dy))
    (f)))

(check (apply interpret dependent-definitions) => '(1 2))

(define scan-out-defines scan-out-defines-simultaneously)

(check-error (apply interpret dependent-definitions))
(check (interpret '(define (f x)
                     (define (even? n) (if (= n 0) true (odd? (- n 1))))
                     (define (odd? n) (if (= n 0) false (even? (- n 1))))
                     (even? x))
                  '(f 10))
       => #t)
(check (interpret '(define (f) (+ 1 2)) '(f)) => 3)
