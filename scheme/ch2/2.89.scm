(define put '())
(define get '())

; Adapted version from https://stackoverflow.com/a/5499256/1461965
(define (define-put-get)
  (define global-entries '())

  (define (make-entry key value) (cons key value))
  (define (entry-key entry) (car entry))
  (define (entry-value entry) (cdr entry))

  (define (put-global key1 key2 value)
    (define (put-local key entries)
      (cond ((null? entries) (list (make-entry key value)))
            ((equal? (entry-key (car entries)) key) (cons (make-entry key value) (cdr entries)))
            (else (cons (car entries) (put-local key (cdr entries))))))
    (set! global-entries (put-local (cons key1 key2) global-entries)))

  (define (get-global key1 key2)
    (define (get-local key entries)
      (cond ((null? entries) '())
            ((equal? (entry-key (car entries)) key) (entry-value (car entries)))
            (else (get-local key (cdr entries)))))
    (let ((value (get-local (cons key1 key2) global-entries)))
      (if (null? value)
        (error "Global lookup failed -- keys" key1 key2)
        value)))

  (set! put put-global)
  (set! get get-global)
'done)

(define-put-get)

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;
; Scheme numbers
;
(define (install-scheme-number-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
'done)

;
; Rational numbers
;
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (eq? (numer x) (numer y)) (eq? (denom x) (denom y))))
  (define (zero? x)
    (= (numer x) 0))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) zero?)
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;
; Polynomials
;
(define (install-polynomial-package)
  (define (left-pad-list len value l)
    (if (< (length l) len)
      (left-pad-list len value (cons value l))
    l))

  (define (adjoin-term term term-list)
    (let ((t1-length (length term))
          (t2-length (length term-list))
          (t1 term)
          (t2 term-list))
      (let ((max-length (max t1-length t2-length)))
        (let ((padded-t1 (left-pad-list max-length 0 t1))
              (padded-t2 (left-pad-list max-length 0 t2)))
          (map
            (lambda (t1-t2-el)
              (+ (car t1-t2-el) (cadr t1-t2-el)))
            (zip padded-t1 padded-t2))))))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (let ((coeff (car term-list))
          (order (length (cdr term-list))))
      (make-term order coeff)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff)
    (if (> order 0)
      (let ((rest-of-coeffs (make-list order 0)))
      (cons coeff rest-of-coeffs))
      (list coeff)))
  (define (order term) (length (cdr term)))
  (define (coeff term) (car term))

  (define (add-terms L1 L2) ; add-terms keeps the property of the term coefficients being sorted
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2) ; this part of 'mul-terms' keeps the property of the term coefficients being sorted
                 (mul-terms (rest-terms L1) L2)))) ; also keeps the coefficients sorted (recursive property)
  (define (mul-term-by-all-terms t1 L) ; mul-term-by-all-terms keeps the property of the term coefficients being sorted
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (negate-terms terms)
    (map
      (lambda (term)
        (make-term
          (order term)
          (- (coeff term))))
      terms))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (zero? p)
    (every
      (lambda (term)
        (= 0 (coeff term)))
      (term-list p)))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero?)
'done)

(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))

(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)

(define p1 (make-poly 'x (list 1 2 1))) ; x^2 + 2x + 1
(define p2 (make-poly 'x (list 1 2 0 1))) ; x^3 + 2x^2 + 1
(define p3 (make-poly 'x (list 1 1 0))) ; x^2 + x

(newline)
(display (add p1 p2)) ; (polynomial x '(1 3 2 2))

(newline)
(display (mul p1 p3)) ; (polynomial x '(1 3 3 1 0))

;(define p-zero1 (make-poly 'x '()))
;(define p-zero2 (make-poly 'x (list '(2 0) '(1 0) '(0 0))))

;(newline)
;(display (=zero? p-zero1)) ; #t

;(newline)
;(display (=zero? p-zero2)) ; #t

;(newline)
;(display (=zero? p1)) ; #f

;(newline)
;(display (sub p2 p1)) ; (polynomial x (3 1) (2 1) (1 -2))

; TODO: Re-implement =zero? and negate-terms to be independent of the representation of the polynomials,
; update the solution for 2.87.88