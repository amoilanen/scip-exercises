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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(install-scheme-number-package)
(install-rational-package)

(newline)
(display (add 2 3)) ; 5

(define r1 (make-rational 2 3))
(define r2 (make-rational 5 7))

(newline)
(display (mul r1 r2)) ; 10 / 21