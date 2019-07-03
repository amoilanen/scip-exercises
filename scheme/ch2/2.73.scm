(define get 2d-get)

(define put 2d-put!)

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((and (number? b) (number? e)) (expt b e))
    ((=number? e 1) b)
    ((=number? e 0) 1)
    (else (list '** b e))))

(put 'deriv '+
  (lambda (operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var))))

(put 'deriv '*
  (lambda (operands var)
    (make-sum
      (make-product (car operands)
                    (deriv (cadr operands) var))
      (make-product (deriv (car operands) var)
                    (cadr operands)))))

(put 'deriv '**
  (lambda (operands var)
    (let ((base (car operands))
          (exponent (cadr operands)))
      (make-product
        (make-product
          exponent
          (make-exponentiation
            base
            (make-sum exponent -1)))
        (deriv base var)))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(newline)
(display (deriv '(+ (* x 3) y) 'x))

(newline)
(display (deriv '(** x 3) 'x))

; a. number? and same-variable? functions depend on the underlying data used in the expression which is not konwn
; at the time when expression is constructed => cannot be used in the data-driven approach
; d. we would need to change the procedure installation code by likewise swapping the order of the arguments