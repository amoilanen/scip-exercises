(load "lib/check.scm")
(load "ch4/lib/mceval.scm")

;; A frame is a headed list of (variable . value) pairs; the header gives
;; add-binding-to-frame! a pair to mutate even when the frame is empty.

(define (make-frame variables values)
  (cons '*frame* (map cons variables values)))
(define (frame-bindings frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (frame-bindings frame))))

(define (frame-binding var frame)
  (assq var (frame-bindings frame)))

(define (environment-binding var env)
  (if (eq? env the-empty-environment)
      #f
      (or (frame-binding var (first-frame env))
          (environment-binding var (enclosing-environment env)))))

(define (lookup-variable-value var env)
  (let ((binding (environment-binding var env)))
    (if binding
        (cdr binding)
        (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((binding (environment-binding var env)))
    (if binding
        (set-cdr! binding val)
        (error "Unbound variable -- SET!" var))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (frame-binding var frame)))
    (if binding
        (set-cdr! binding val)
        (add-binding-to-frame! var val frame))))

(check (make-frame '(a b) '(1 2)) => '(*frame* (a . 1) (b . 2)))
(check (let ((frame (make-frame '() '())))
         (add-binding-to-frame! 'a 1 frame)
         frame)
       => '(*frame* (a . 1)))

(check (interpret '(define x 1) '(define x 2) 'x) => 2)
(check (interpret '(define x 1) '(set! x (+ x 1)) 'x) => 2)
(check (interpret '(define x 'outer)
                  '(define (f x) (set! x 'changed) x)
                  '(list (f 'inner) x))
       => '(changed outer))
(check (interpret '(define (make-counter)
                     (define count 0)
                     (lambda () (set! count (+ count 1)) count))
                  '(define counter (make-counter))
                  '(counter)
                  '(counter))
       => 2)
(check-error (interpret 'unbound))
(check-error (interpret '(set! unbound 1)))
