(load "lib/check.scm")
(load "ch5/lib/compiler.scm")
(load "ch5/5.39.scm")

;; The compile-time environment is a list of frames, each a list of the
;; variables of a lambda.  Instead of being passed to compile and to every
;; code generator, it is a parameter rebound while the body of a lambda is
;; compiled.  The body is compiled entirely within that dynamic extent, and
;; nothing else is, so every expression sees the same environment as it
;; would with the extra argument.
(define compile-time-environment (make-parameter '()))

(define (extend-compile-time-environment variables ct-env)
  (cons variables ct-env))

(define compile-lambda-body-without-environment compile-lambda-body)

(define (compile-lambda-body exp proc-entry)
  (parameterize ((compile-time-environment
                  (extend-compile-time-environment
                   (lambda-parameters exp)
                   (compile-time-environment))))
    (compile-lambda-body-without-environment exp proc-entry)))

(define (variable-environments exp)
  (let ((compile-plain-variable compile-variable)
        (environments '()))
    (fluid-let ((compile-variable
                 (lambda (var target linkage)
                   (set! environments
                         (cons (list var (compile-time-environment))
                               environments))
                   (compile-plain-variable var target linkage))))
      (compile exp 'val 'next))
    environments))

(define environments
  (variable-environments
   '((lambda (x y)
       (lambda (a b c d e)
         ((lambda (y z) (* x y z))
          (* a b x)
          (+ c d x))))
     3
     4)))

(check (assq 'z environments) => '(z ((y z) (a b c d e) (x y))))
(check (assq 'c environments) => '(c ((a b c d e) (x y))))
(check (assq '+ environments) => '(+ ((a b c d e) (x y))))
(check (compile-time-environment) => '())
(check (variable-environments 'x) => '((x ())))
