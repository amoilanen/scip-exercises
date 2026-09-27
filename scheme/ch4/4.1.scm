(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/mceval.scm" (current-load-pathname)))

;; Binding one operand with let before the other is evaluated fixes the order,
;; whatever order the underlying Scheme uses for the arguments of cons.

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (mc-eval (first-operand exps) env)))
        (cons first
              (list-of-values-left-to-right (rest-operands exps) env)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values-right-to-left (rest-operands exps) env)))
        (cons (mc-eval (first-operand exps) env)
              rest))))

(define (evaluation-order)
  (reverse
   (interpret '(define order '())
              '(define (note x) (set! order (cons x order)) x)
              '(list (note 1) (note 2) (note 3))
              'order)))

(define (arguments-value)
  (interpret '(list 1 (+ 1 1) 3)))

(define list-of-values list-of-values-left-to-right)
(check (evaluation-order) => '(1 2 3))
(check (arguments-value) => '(1 2 3))

(define list-of-values list-of-values-right-to-left)
(check (evaluation-order) => '(3 2 1))
(check (arguments-value) => '(1 2 3))
