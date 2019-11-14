(define put '())
(define get '())

; Adapted version from https://stackoverflow.com/a/5499256/1461965
(define (define-put-get)
  (define global-entries '())

  (define (make-entry key value) (cons key value))
  (define (entry-key entry) (car entry))
  (define (entry-value entry) (cdr entry))

  (define (put-global key1 key2 value)
    (define (put-local key value entries)
      (cond ((null? entries) (list (make-entry key value)))
            ((equal? (entry-key (car entries)) key) (cons (make-entry key value) (cdr entries)))
            (else (cons (car entries) (put-local key value (cdr entries))))))
    (set! global-entries (put-local (cons key1 key2) value global-entries)))

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

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (pairs-of list)
  (define (pairs-with item rest-of-items)
    (append
      (map (lambda (x) (cons item x)) rest-of-items)
      (pairs-of rest-of-items)))
  (if (null? list)
      '()
      (pairs-with (car list) (cdr list))))

(define (build-ordering relations)
  (define (last-element list) (car (reverse list)))
  (define (segment-start segment)
    (car segment))
  (define (segment-end segment)
    (cadr segment))
  (define (segment-elements segment)
    (caddr segment))
  (define (make-segment start end elements)
    (list start end elements))
  (define (segments-can-be-merged first second)
    (or
      (eq? (segment-end first) (segment-start second))
      (eq? (segment-end second) (segment-start first))))
  (define (merge-two-segments first second)
    (cond
      ((eq? (segment-end first) (segment-start second))
        (make-segment
          (segment-start first)
          (segment-end second)
          (append (segment-elements first) (cdr (segment-elements second)))))
      ((eq? (segment-end second) (segment-start first))
        (make-segment
          (segment-start second)
          (segment-end first)
          (append (segment-elements second) (cdr (segment-elements first)))))
      (else error "Cannot merge segments" first second)))

  (define (merge-segments segments)
    (let ((merged-segments (try-merging segments)))
      (if (= (length merged-segments) (length segments))
        segments
        (merge-segments merged-segments))))
  (define (try-merging segments)
    (let ((segment-pairs (pairs-of segments)))
      (let ((mergable-segment-pair
             (find (lambda (pair)
                     (segments-can-be-merged (car pair) (cdr pair)))
                   segment-pairs)))
        (if mergable-segment-pair
            (let ((first-segment (car mergable-segment-pair))
                  (second-segment (cdr mergable-segment-pair)))
              (let ((rest-of-segments
                     (filter
                       (lambda (segment)
                         (and (not (eq? segment first-segment)) (not (eq? segment second-segment))))
                       segments)))
                (cons (merge-two-segments first-segment second-segment) rest-of-segments)))
            segments))))
  (define (to-relations segments)
    (map segment-elements segments))
  (let ((segments (map (lambda (relation) (make-segment (car relation) (last-element relation) relation)) relations)))
    (to-relations (merge-segments segments))))

; (list '(integer rational real complex) '(c b a))
(define type-hierarchies '())

(define (define-subtype-of . type-relation)
  (set! type-hierarchies
        (build-ordering
           (cons type-relation type-hierarchies))))

(define (exists? elements predicate)
  (cond ((null? elements) #f)
        ((predicate (car elements)) #t)
        (else (exists? (cdr elements) predicate))))

(define (index-of-element elements element)
  (define (index-in-sublist elements element current-index)
    (cond ((null? elements) -1)
          ((eq? (car elements) element) current-index)
          (else (index-in-sublist (cdr elements) element (+ 1 current-index)))))
  (index-in-sublist elements element 0))

(define (is-greater-type type1 type2)
  (exists? type-hierarchies
    (lambda (type-hierarchy)
      (> (index-of-element type-hierarchy type1) (index-of-element type-hierarchy type2)))))

(define (coerce . args)
  ;(newline)
  ;(display 'coerce)
  ;(newline)
  ;(display args)
  (define (coerce-to type arg)
    (if (eq? (type-tag arg) type)
        arg
        (let ((raised-arg (raise arg)))
          (if (eq? (type-tag arg) (type-tag raised-arg))
            (error "Cannot coerce" (type-tag arg) 'to type)
            (coerce-to type raised-arg)))))
  (let ((greatest-type (car (sort (map type-tag args) is-greater-type))))
    (map
      (lambda (t) (coerce-to greatest-type t))
      args)))

(define (drop x)
  ;(newline)
  ;(display 'drop)
  ;(newline)
  ;(display x)
  (if (pair? x)
    (let ((projection (project x)))
      (let ((raised-projection (raise projection)))
        (if (and (equ? x raised-projection) (not (eq? (type-tag projection) (type-tag x))))
          (drop projection)
          x)))
    x))

(define (apply-generic op . args)
  ;(newline)
  ;(display 'apply-generic)
  ;(newline)
  ;(display (cons op args))
  (let ((coerced-args (apply coerce args)))
    (let ((type-tags (map type-tag coerced-args)))
      (let ((proc (get op type-tags)))
        (if proc
            (let ((result (apply proc (map contents coerced-args))))
              (if (or (eq? op 'raise) (eq? op 'project) (eq? op 'equ?))
                result
                (drop result)))
            (let ((raised-args (map raise coerced-args)))
              (if (eq?
                      (type-tag (car coerced-args))
                      (type-tag (car raised-args)))
                (error
                  "No method for these types -- APPLY-GENERIC"
                  (list op type-tags))
                (apply apply-generic (cons op raised-args)))))))))

;
; Complex numbers
;
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  (define (project z)
    (make-real (real-part z)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) equ?)
  (put 'project '(complex) project)
'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;
; Integer numbers
;
(define (install-integer-package)
  (define (tag n)
    (attach-tag 'integer n))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put 'project '(integer)
       (lambda (x) (make-integer x)))
  (put 'add '(integer integer)
       (lambda (n m) (tag (+ n m))))
  (put 'sub '(integer integer)
       (lambda (n m) (tag (- n m))))
  (put 'mul '(integer integer)
       (lambda (n m) (tag (* n m))))
  (put 'div '(integer integer)
       (lambda (n m) (tag (/ n m))))
  (put 'make 'integer
       (lambda (n) (tag (inexact->exact n))))
  (define (integer->rational n)
    (make-rational n 1))
  (define-subtype-of 'integer 'rational)
  (put 'raise '(integer) integer->rational)
'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;
; Real numbers
;
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put 'project '(real)
       (lambda (x)
         (let ((r (rationalize (inexact->exact x) 1/100)))
           (make-rational
             (numerator r) (denominator r)))))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (exact->inexact x))))
  (define (real->complex x)
    (make-complex-from-real-imag x 0))
  (define-subtype-of 'real 'complex)
  (put 'raise '(real) real->complex)
'done)

(define (make-real x)
  ((get 'make 'real) x))

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
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (define (project x)
    (make-integer (numer x)))
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
  (define (rational->real r)
    (let ((n (car r))
          (d (cdr r)))
        (make-real (/ n d))))
  (define-subtype-of 'rational 'real)
  (put 'raise '(rational) rational->real)
  (put 'equ? '(rational rational) equ?)
  (put 'project '(rational) project)
'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (equ? x y) (apply-generic 'equ? x y))

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)

(define n1 (make-integer 2))
(define n2 (make-integer 3))

(newline)
(display (add n1 n2)) ; 5

(define r1 (make-rational 2 3))
(define r2 (make-rational 5 7))

(newline)
(display (mul r1 r2)) ; 10 / 21

(define x1 (make-real 1.23))
(define x2 (make-real 2.34))

(newline)
(display (add x1 x2)) ; 3.57

(define c1 (make-complex-from-real-imag 1 2))
(define c2 (make-complex-from-real-imag 3 4))

(newline)
(display (add c1 c2)) ; 4 + 6i

(newline)
(display (raise n1))
(newline)
(display (raise r1))
(newline)
(display (raise x1))

; Automatically raise to the most generic type
(newline)
(display (add n1 c1)) ; 3 + 2i

; Try raising the argument if the operation is not available on its type
(newline)
(display (real-part n1)) ; 2

; Simplified example with the types a, b, c, d

(define (make-a x)
  (attach-tag 'a x))

(define (make-b x)
  (attach-tag 'b x))

(define (make-c x)
  (attach-tag 'c x))

(define (make-d x)
  (attach-tag 'd x))

(define-subtype-of 'c 'b)
(define-subtype-of 'b 'a)

(put 'raise '(b)
  (lambda (x) (make-a x)))
(put 'raise '(c)
  (lambda (x) (make-b x)))

(newline)
(display (raise (make-b 1)))
(newline)
(display (raise (make-c 2)))

(newline)
(display (index-of-element (list 1 2 3) 2))
(newline)
(display (index-of-element (list 1 2 3) 4))
(newline)
(display (is-greater-type 'b 'c))
(newline)
(display (is-greater-type 'b 'a))

(newline)
(display (sort '(b a c) is-greater-type))

(newline)
(display (coerce (make-a 1) (make-b 2) (make-c 3))); '('(a 1) '(a 2) '(a 3))

; Cannot coerce d to a
;(newline)
;(display (coerce (make-a 1) (make-d 2) (make-c 3))); '('(a 1) '(a 2) '(a 3))

(newline)
(display (drop
  (make-complex-from-real-imag 2 3)))

(newline)
(display (drop
  (make-complex-from-real-imag 3 0)))

;2.85 apply-generic should use drop
(define 1plusi (make-complex-from-real-imag 1 1))
(define 1minusi (make-complex-from-real-imag 1 -1))

(newline)
(display '(apply-generic drop))

(newline)
(display (add 1plusi 1minusi))

(newline)
(display (mul 1plusi 1minusi))

(newline)
(display (sub 1plusi 1minusi))