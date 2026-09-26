(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

;; The iterative factorial machine of exercise 5.1 in the full language of
;; figure 5.3: data paths name every button, and the controller pushes them.

(define factorial-data-paths
  '(data-paths
    (registers
     ((name n))
     ((name product)
      (buttons ((name p<-1) (source (constant 1)))
               ((name p<-mul) (source (operation mul)))))
     ((name counter)
      (buttons ((name c<-1) (source (constant 1)))
               ((name c<-add) (source (operation add))))))
    (operations
     ((name mul) (inputs (register product) (register counter)))
     ((name add) (inputs (register counter) (constant 1)))
     ((name >) (inputs (register counter) (register n))))))

(define factorial-controller-description
  '(controller
      (p<-1)
      (c<-1)
    test-counter
      (test >)
      (branch (label fact-done))
      (p<-mul)
      (c<-add)
      (goto (label test-counter))
    fact-done))

;; The same controller in the abbreviated notation that the simulator runs.
(define factorial-iter-controller
  '((assign product (const 1))
    (assign counter (const 1))
    test-counter
      (test (op >) (reg counter) (reg n))
      (branch (label fact-done))
      (assign product (op mul) (reg product) (reg counter))
      (assign counter (op add) (reg counter) (const 1))
      (goto (label test-counter))
    fact-done))

;; To check that the two descriptions agree, expand the full one into the
;; abbreviated one: each button becomes an assign, each test names its inputs.

(define (description-items description) (cdr description))
(define (item-name item) (cadr (assq 'name item)))

(define (source->expression source operations)
  (case (car source)
    ((constant) (list (list 'const (cadr source))))
    ((register) (list (list 'reg (cadr source))))
    ((operation) (operation-expression (cadr source) operations))
    (else (error "Unknown source" source))))

(define (operation-expression name operations)
  (let ((operation (find (lambda (op) (eq? (item-name op) name)) operations)))
    (if (not operation)
        (error "Unknown operation" name))
    (cons (list 'op name)
          (map (lambda (input) (car (source->expression input operations)))
               (cdr (assq 'inputs operation))))))

(define (register-buttons registers operations)
  (append-map
   (lambda (register)
     (map (lambda (button)
            (list (item-name button)
                  (cons* 'assign
                         (item-name register)
                         (source->expression (cadr (assq 'source button))
                                             operations))))
          (let ((buttons (assq 'buttons register)))
            (if buttons (cdr buttons) '()))))
   registers))

(define (expand-controller data-paths controller)
  (let* ((operations (description-items
                      (assq 'operations (description-items data-paths))))
         (registers (description-items
                     (assq 'registers (description-items data-paths))))
         (buttons (register-buttons registers operations)))
    (define (expand item)
      (cond ((symbol? item) item)
            ((eq? (car item) 'test)
             (cons 'test (operation-expression (cadr item) operations)))
            ((assq (car item) buttons) => cadr)
            (else item)))
    (map expand (description-items controller))))

(check (expand-controller factorial-data-paths
                          factorial-controller-description)
       => factorial-iter-controller)

(define (factorial n)
  (run-machine (make-machine '(n product counter)
                             (list (list 'mul *) (list 'add +) (list '> >))
                             factorial-iter-controller)
               (list (list 'n n))
               'product))

(check (factorial 1) => 1)
(check (factorial 6) => 720)
