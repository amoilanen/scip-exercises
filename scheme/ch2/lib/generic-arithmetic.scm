;; The generic arithmetic system of section 2.5 with the sparse polynomials
;; of 2.5.3.  Scheme numbers are untagged (exercise 2.78).
;;
;; Packages register lambdas that call top-level procedures, so an exercise
;; changes a package by redefining those procedures, or extends it with put.

(define operation-table (make-equal-hash-table))

(define (put op type item)
  (hash-table-set! operation-table (list op type) item))

(define (get op type)
  (hash-table-ref/default operation-table (list op type) #f))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (negate x) (apply-generic 'negate x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;;; Scheme numbers

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'negate '(scheme-number) -)
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) zero?)
  'done)

;;; Rational numbers

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (negate-rat x)
  (make-rat (- (numer x)) (denom x)))

(define (equ-rat? x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(define (zero-rat? x)
  (= (numer x) 0))

(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'negate '(rational) (lambda (x) (tag (negate-rat x))))
  (put 'equ? '(rational rational) (lambda (x y) (equ-rat? x y)))
  (put '=zero? '(rational) (lambda (x) (zero-rat? x)))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; Sparse term lists: terms of decreasing order, no zero coefficients

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (else
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1 (add-terms (rest-terms l1) l2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2 (add-terms l1 (rest-terms l2))))
                 (else
                  (adjoin-term (make-term (order t1)
                                          (add (coeff t1) (coeff t2)))
                               (add-terms (rest-terms l1)
                                          (rest-terms l2)))))))))

(define (negate-terms l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t (first-term l)))
        (adjoin-term (make-term (order t) (negate (coeff t)))
                     (negate-terms (rest-terms l))))))

(define (sub-terms l1 l2)
  (add-terms l1 (negate-terms l2)))

(define (mul-term-by-all-terms t1 l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term (make-term (+ (order t1) (order t2))
                                (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-terms t1 (rest-terms l))))))

(define (mul-terms l1 l2)
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))

(define (zero-terms? l)
  (or (empty-termlist? l)
      (and (=zero? (coeff (first-term l)))
           (zero-terms? (rest-terms l)))))

;;; Polynomials

(define (make-poly variable term-list) (cons variable term-list))
(define (variable p) (car p))
(define (term-list p) (cdr p))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (common-variable p1 p2 caller)
  (if (same-variable? (variable p1) (variable p2))
      (variable p1)
      (error "Polys not in same var --" caller p1 p2)))

(define (add-poly p1 p2)
  (make-poly (common-variable p1 p2 'add-poly)
             (add-terms (term-list p1) (term-list p2))))

(define (sub-poly p1 p2)
  (make-poly (common-variable p1 p2 'sub-poly)
             (sub-terms (term-list p1) (term-list p2))))

(define (mul-poly p1 p2)
  (make-poly (common-variable p1 p2 'mul-poly)
             (mul-terms (term-list p1) (term-list p2))))

(define (negate-poly p)
  (make-poly (variable p) (negate-terms (term-list p))))

(define (zero-poly? p)
  (zero-terms? (term-list p)))

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2) (zero-poly? (sub-poly p1 p2))))
  (put '=zero? '(polynomial) (lambda (p) (zero-poly? p)))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)
