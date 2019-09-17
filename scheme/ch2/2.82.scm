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
        #f
        value)))

  (set! put put-global)
  (set! get get-global)
'done)

(define-put-get)

(define (put-coercion type1 type2 coercion)
  (put type1 type2 coercion))

(define (get-coercion type1 type2)
  (get type1 type2))

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

(define (coerce . args)
  (define (coerce-to-type type-to-try other-types-to-try args)
    (define coercions
      (map
        (lambda (arg)
          (if (eq? type-to-try (type-tag arg))
            (lambda (x) x)
            (get-coercion (type-tag arg) type-to-try)))
        args))
    (if (member #f coercions)
      (if (null? other-types-to-try)
        #f
        (coerce-to-type (car other-types-to-try) (cdr other-types-to-try) args))
      (let ((coercions-args (map cons coercions args)))
        (map
          (lambda (coercion-arg)
            ((car coercion-arg) (cdr coercion-arg)))
          coercions-args))))
  (define arg-types (map type-tag args))
  (coerce-to-type (car arg-types) (cdr arg-types) args))

; TODO Call coerce function instead of the hardcoded two argument handling
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                        (cond
                          (t1->t2
                            (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                            (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                (list op type-tags)))))
                    (error "No method for these types"
                                (list op type-tags))))
              (error "No method for these types"
                                (list op type-tags)))))))

; Type pyramide, from most to least generic type: a > b > c

(define (make-a x)
  (attach-tag 'a x))

(define (make-b x)
  (attach-tag 'b x))

(define (make-c x)
  (attach-tag 'c x))

(define (make-d x)
  (attach-tag 'd x))

(put-coercion 'c 'b
  (lambda (x) (make-b (contents x))))
(put-coercion 'c 'a
  (lambda (x) (make-a (contents x))))
(put-coercion 'b 'a
  (lambda (x) (make-a (contents x))))

(put 'custom-op '(c c c)
  (lambda (x y z) (+ x y z)))
(put 'custom-op '(b b b)
  (lambda (x y z) (* x y z)))
(put 'custom-op '(a b b)
  (lambda (x y z) (- x y z)))
(define (custom-op x y z) (apply-generic 'custom-op x y z))

; coerce (a, b, c) -> (a, a, a)
(newline)
(display (coerce (make-a 1) (make-b 2) (make-c 3))); '('(a 1) '(a 2) '(a 3))

; coerce (c, b, c) -> (b, b, b)
(newline)
(display (coerce (make-c 1) (make-b 2) (make-c 3))); '('(b 1) '(b 2) '(b 3))

; coerce (b, c, a) -> (a, a, a)
(newline)
(display (coerce (make-b 1) (make-c 2) (make-a 3))); '('(a 1) '(a 2) '(a 3))

; coerce (b, c, d) -> #f
(newline)
(display (coerce (make-b 1) (make-c 2) (make-d 3))); #f

; Generic operation on (c, c, c)
(newline)
(display (custom-op (make-c 2) (make-c 3) (make-c 4))) ; 9

; Coercion c -> b, (b, c, c) -> (b, b, b)
(newline)
(display (custom-op (make-b 2) (make-c 3) (make-c 4))) ; 24

; There is another more appropriate conversion
; c -> a, b-> a are used, (a, b, c) -> (a, a, a) for which there is no custom-op
; instead would have been better
; c -> b, (a, b, c) -> (a, b, b) for which there is a custom-op
(newline)
(display (custom-op (make-a 6) (make-b 3) (make-c 2)))
; throws error "No method for '(a a a)", instead of printing 6 - 3 - 2 = 1