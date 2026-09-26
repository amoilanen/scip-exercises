;; Exercise 5.52: the compiler of section 5.5 with a back end that writes C.
;;
;; The front end is the book's compiler, extended with let, named let, and,
;; or and boolean constants. Each instruction of the register-machine code
;; it produces becomes a C statement: registers are fields of reg, labels
;; are C labels, and a goto to a label held in a register jumps through a
;; switch on the label's number. runtime.c supplies the operations, and
;; the data layer of exercise 5.51 the objects and garbage collection.
;;
;;   (compile-to-c exps port)  writes a C program that evaluates exps

(load "ch5/lib/compiler.scm")

;;; Derived expressions

(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (and (let? exp) (symbol? (cadr exp))))

;; (let name ((v e) ...) body) is (((lambda () (define (name v ...) body)
;; name)) e ...), so the body sees name but the e's do not.
(define (let->combination exp)
  (let* ((named? (named-let? exp))
         (bindings (if named? (caddr exp) (cadr exp)))
         (body (if named? (cdddr exp) (cddr exp)))
         (variables (map car bindings))
         (operands (map cadr bindings)))
    (if named?
        (let ((name (cadr exp)))
          (cons (list (make-lambda
                       '()
                       (list (cons 'define (cons (cons name variables) body))
                             name)))
                operands))
        (cons (make-lambda variables body) operands))))

(define (and->if exps)
  (cond ((null? exps) #t)
        ((null? (cdr exps)) (car exps))
        (else (make-if (car exps) (and->if (cdr exps)) #f))))

;; The rest of the or is wrapped in a procedure, so that the variables
;; value and rest cannot capture variables of the program.
(define (or->combination exps)
  (cond ((null? exps) #f)
        ((null? (cdr exps)) (car exps))
        (else
         `((lambda (value rest) (if value value (rest)))
           ,(car exps)
           (lambda () ,(or->combination (cdr exps)))))))

(define book-compile compile)

(define (compile exp target linkage)
  (cond ((boolean? exp) (compile-constant exp target linkage))
        ((let? exp) (compile (let->combination exp) target linkage))
        ((tagged-list? exp 'and)
         (compile (and->if (cdr exp)) target linkage))
        ((tagged-list? exp 'or)
         (compile (or->combination (cdr exp)) target linkage))
        (else (book-compile exp target linkage))))
