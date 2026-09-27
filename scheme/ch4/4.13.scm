(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/mceval.scm" (current-load-pathname)))

;; (make-unbound! var) removes the binding of var from the first frame of the
;; environment only, and it is an error if var is not bound there. Like
;; define, it acts on the current frame: letting it reach into enclosing
;; frames would allow a procedure to destroy bindings it does not own, and
;; would make the result depend on where the procedure is called from.

(define (make-unbound? exp) (tagged-list? exp 'make-unbound!))
(define (make-unbound-variable exp) (cadr exp))

(define (remove-binding-from-frame! var frame)
  (let scan ((vars (frame-variables frame))
             (vals (frame-values frame))
             (vars-before '())
             (vals-before '()))
    (cond ((null? vars)
           (error "Variable not bound in this frame -- MAKE-UNBOUND!" var))
          ((eq? var (car vars))
           (set-car! frame (append-reverse vars-before (cdr vars)))
           (set-cdr! frame (append-reverse vals-before (cdr vals))))
          (else
           (scan (cdr vars)
                 (cdr vals)
                 (cons (car vars) vars-before)
                 (cons (car vals) vals-before))))))

(define (eval-make-unbound exp env)
  (remove-binding-from-frame! (make-unbound-variable exp) (first-frame env))
  'ok)

(define eval-without-make-unbound mc-eval)

(define (mc-eval exp env)
  (if (make-unbound? exp)
      (eval-make-unbound exp env)
      (eval-without-make-unbound exp env)))

(check (let ((frame (make-frame '(a b c) '(1 2 3))))
         (remove-binding-from-frame! 'b frame)
         frame)
       => '((a c) 1 3))
(check-error (interpret '(define x 1) '(make-unbound! x) 'x))
(check (interpret '(define a 1)
                  '(define b 2)
                  '(define c 3)
                  '(make-unbound! b)
                  '(list a c))
       => '(1 3))
(check (interpret '(define x 'outer)
                  '(define (f x) (make-unbound! x) x)
                  '(f 'inner))
       => 'outer)
(check-error (interpret '(define x 1)
                        '(define (f) (make-unbound! x))
                        '(f)))
(check-error (interpret '(make-unbound! never-defined)))
