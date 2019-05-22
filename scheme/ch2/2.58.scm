(define (make-tree left right op)
  (list left right op))

(define (left-of-tree tree)
  (car tree))

(define (right-of-tree tree)
  (cadr tree))

(define (op-of-tree tree)
  (caddr tree))

(define (precedence op)
  (cond ((equal? op '+) 1)
        ((equal? op '*) 2)))

(define (is-operator? ch)
  (or (equal? ch '+) (equal? ch '*)))

(define (push-operand x output)
  (cons x output))

(define (push-operator op output)
  (let ((left (car output))
        (right (cadr output))
        (popped-output (cddr output)))
    (cons (make-tree left right op) popped-output)))

(define (push-all-operators ops output)
  (if (null? ops)
    output
    (push-all-operators
      (cdr ops)
      (push-operator (car ops) output))))

; Simplified Shunting-Yard algorithm taking advantage of Scheme expressions
; already being lists of lists: no need to emulate recursive invocation by special handling of ( and )
(define (build-ast infix-expr operators output)
  (if (null? infix-expr)
    (car (push-all-operators operators output))
    (let ((current-expr (car infix-expr)))
      (cond ((is-operator? current-expr)
              (if (and
                      (pair? operators)
                      (is-operator? (car operators))
                      (>= (precedence (car operators)) (precedence current-expr)))
                  (build-ast infix-expr (cdr operators) (push-operator (car operators) output))
                  (build-ast (cdr infix-expr) (cons current-expr operators) output)))
            ((pair? current-expr)
              (let ((current-output (build-ast current-expr '() '())))
                (build-ast (cdr infix-expr) operators (cons current-output output))))
            (else
              (build-ast (cdr infix-expr) operators (push-operand current-expr output)))))))

(define (to-infix-form ast)
  (if (not (pair? ast))
    ast
    (list
      (to-infix-form (left-of-tree ast))
      (op-of-tree ast)
      (to-infix-form (right-of-tree ast)))))

(newline)
(display (build-ast '(a + b + c) '() '()))

(newline)
(display (to-infix-form (build-ast '(a + b * c + ( d + e ) * f) '() '())))

(newline)
(display (to-infix-form (build-ast '(x * 3) '() '())))

(define (set-parentheses expr)
  (to-infix-form (build-ast expr '() '())))

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+' a2))))

; '(a + b)
(define (sum? x)
  (let ((x-par (set-parentheses x)))
    (and (pair? x-par) (not (null? (cddr x-par))) (eq? (cadr x-par) '+))))

(define (addend s)
  (let ((s-par (set-parentheses s)))
    (car s-par)))

(define (augend s)
  (let ((s-par (set-parentheses s)))
    (caddr s-par)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; '(a * b)
(define (product? x)
  (let ((x-par (set-parentheses x)))
    (and (pair? x-par) (not (null? (cddr x-par))) (eq? (cadr x-par) '*))))

(define (multiplier p)
  (let ((p-par (set-parentheses p)))
    (car p-par)))

(define (multiplicand p)
  (let ((p-par (set-parentheses p)))
    (caddr p-par)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(newline)
(display (deriv '(x * 3) 'x))

(newline)
(display (deriv '(x + (3 * (x + (y + 2)))) 'x))

(newline)
(display (deriv '(x + 3 * (x + y + 2)) 'x))