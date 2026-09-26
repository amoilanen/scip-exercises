(load "lib/check.scm")
(load "ch2/lib/generic-arithmetic.scm")

;; Variables are ordered alphabetically.  A polynomial in v is kept in a
;; canonical form: its coefficients are numbers or polynomials in variables
;; that come after v, and it has a term of positive order (otherwise it is
;; replaced by its constant term).  A number or a polynomial in a later
;; variable is combined with a polynomial in v as a constant polynomial in v.
;; The canonical form is unique, so equal polynomials are equal?.

(define (variable-precedes? v1 v2)
  (string<? (symbol->string v1) (symbol->string v2)))

(define (install-multivariate-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (define (single-term-poly var order coeff)
    (make-poly var (adjoin-term (make-term order coeff)
                                (the-empty-termlist))))
  (define (constant-poly var c)
    (single-term-poly var 0 c))
  (define (simplify p)
    (let ((terms (term-list p)))
      (cond ((empty-termlist? terms) 0)
            ((= (order (first-term terms)) 0) (coeff (first-term terms)))
            (else (tag p)))))
  (define (combine terms-op p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (cond ((same-variable? v1 v2)
             (simplify (make-poly v1 (terms-op (term-list p1)
                                               (term-list p2)))))
            ((variable-precedes? v1 v2)
             (combine terms-op p1 (constant-poly v1 (tag p2))))
            (else
             (combine terms-op (constant-poly v2 (tag p1)) p2)))))
  (define (install-operation op terms-op)
    (put op '(polynomial polynomial)
         (lambda (p1 p2) (combine terms-op p1 p2)))
    (put op '(polynomial scheme-number)
         (lambda (p n) (combine terms-op p (constant-poly (variable p) n))))
    (put op '(scheme-number polynomial)
         (lambda (n p) (combine terms-op (constant-poly (variable p) n) p))))
  ;; Rebuilding the polynomial as a sum of coeff * var^order puts it into
  ;; canonical form, whatever variables its coefficients are in.
  (define (make-canonical var terms)
    (fold-left (lambda (sum term)
                 (add sum
                      (mul (coeff term)
                           (tag (single-term-poly var (order term) 1)))))
               0
               terms))
  (install-operation 'add add-terms)
  (install-operation 'sub sub-terms)
  (install-operation 'mul mul-terms)
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2) (=zero? (combine sub-terms p1 p2))))
  (put 'make 'polynomial make-canonical)
  'done)

(install-multivariate-polynomial-package)

(define x (make-polynomial 'x '((1 1))))
(define y (make-polynomial 'y '((1 1))))
(define z (make-polynomial 'z '((1 1))))

(check x => '(polynomial x (1 1)))
(check (make-polynomial 'x '((0 5))) => 5)
(check (make-polynomial 'x '()) => 0)

(check (add x y) => '(polynomial x (1 1) (0 (polynomial y (1 1)))))
(check (add y x) => (add x y))
(check (add x 3) => '(polynomial x (1 1) (0 3)))
(check (sub 3 x) => '(polynomial x (1 -1) (0 3)))
(check (mul 2 (mul y x)) => '(polynomial x (1 (polynomial y (1 2)))))
(check (mul x 0) => 0)

(check (mul (add x y) (sub x y))
       => '(polynomial x (2 1) (0 (polynomial y (2 -1)))))
(check (sub (add x y) x) => y)
(check (sub (mul x y) (mul y x)) => 0)
(check (mul (mul z y) x) => (mul x (mul y z)))
(check (mul x (mul y z))
       => '(polynomial x (1 (polynomial y (1 (polynomial z (1 1)))))))

(check (make-polynomial 'y (list (list 1 (add x 1)) (list 0 (add x 1))))
       => '(polynomial x
                       (1 (polynomial y (1 1) (0 1)))
                       (0 (polynomial y (1 1) (0 1)))))
(check (make-polynomial 'y (list (list 2 x) (list 1 z)))
       => (add (mul x (mul y y)) (mul y z)))

(check (equ? (mul (add x y) (add x y))
             (add (mul x x) (add (mul 2 (mul x y)) (mul y y))))
       => #t)
(check (equ? (add x y) (add x z)) => #f)
