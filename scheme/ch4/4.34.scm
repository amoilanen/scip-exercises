(load "lib/check.scm")
(load "ch4/lib/lazy.scm")

;; A lazy pair is a tagged object that holds two procedures of the evaluated
;; language, each returning one (delayed) component, so that the printer
;; can recognise it.  Infinite lists, and lists nested infinitely deep, are
;; printed only up to a fixed number of elements, followed by "...".

(define (make-lazy-pair car-getter cdr-getter)
  (list 'lazy-pair car-getter cdr-getter))
(define (lazy-pair? obj) (tagged-list? obj 'lazy-pair))

(define (lazy-pair-car-getter pair)
  (if (lazy-pair? pair)
      (cadr pair)
      (error "Not a lazy pair -- CAR" pair)))

(define (lazy-pair-cdr-getter pair)
  (if (lazy-pair? pair)
      (caddr pair)
      (error "Not a lazy pair -- CDR" pair)))

(define primitive-procedures
  (append (list (list 'make-lazy-pair make-lazy-pair)
                (list 'lazy-pair-car-getter lazy-pair-car-getter)
                (list 'lazy-pair-cdr-getter lazy-pair-cdr-getter))
          primitive-procedures))

(define lazy-pair-definitions
  '((define (cons x y)
      (make-lazy-pair (lambda () x) (lambda () y)))
    (define (car pair) ((lazy-pair-car-getter pair)))
    (define (cdr pair) ((lazy-pair-cdr-getter pair)))
    (define (add-lists list1 list2)
      (cons (+ (car list1) (car list2))
            (add-lists (cdr list1) (cdr list2))))
    (define ones (cons 1 ones))
    (define integers (cons 1 (add-lists ones integers)))))

(define (call-getter getter)
  (force-it (mc-apply getter '() the-empty-environment)))

(define (lazy-car pair) (call-getter (lazy-pair-car-getter pair)))
(define (lazy-cdr pair) (call-getter (lazy-pair-cdr-getter pair)))

(define max-printed-elements 10)

(define (print-lazy-pair pair)
  (let ((remaining max-printed-elements))
    (define (print-object object)
      (if (lazy-pair? object)
          (begin (display "(")
                 (print-elements object)
                 (display ")"))
          (user-print object)))
    (define (print-elements pair)
      (if (= remaining 0)
          (display "...")
          (begin
            (set! remaining (- remaining 1))
            (print-object (lazy-car pair))
            (print-tail (lazy-cdr pair)))))
    (define (print-tail rest)
      (cond ((lazy-pair? rest)
             (display " ")
             (print-elements rest))
            ((not (null? rest))
             (display " . ")
             (print-object rest))))
    (print-object pair)))

(define (user-print object)
  (cond ((lazy-pair? object) (print-lazy-pair object))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

(define (printed . exps)
  (with-output-to-string
    (lambda ()
      (user-print (apply interpret (append lazy-pair-definitions exps))))))

(check (printed '42) => "42")
(check (printed ''(a b)) => "(a b)")
(check (printed '(cons 1 (cons 2 '()))) => "(1 2)")
(check (printed '(cons 1 2)) => "(1 . 2)")
(check (printed '(cons (cons 1 '()) (cons 2 '()))) => "((1) 2)")
(check (printed '(cons (lambda (x) x) '()))
       => "((compound-procedure (x) (x) <procedure-env>))")
(check (printed 'ones) => "(1 1 1 1 1 1 1 1 1 1 ...)")
(check (printed '(cdr (cdr integers))) => "(3 4 5 6 7 8 9 10 11 12 ...)")
(check (printed '(define nested (cons nested '())) 'nested)
       => (string-append (make-string 11 #\() "..." (make-string 11 #\))))
(check (printed '(car (cdr (cons 1 (cons 2 (car '())))))) => "2")
(check-error (printed '(car 1)))
