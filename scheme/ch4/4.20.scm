(load "lib/check.scm")
(load "ch4/4.16.scm")

;;; a.

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (make-let (map (lambda (variable) (list variable ''*unassigned*))
                 (let-variables exp))
            (append (map (lambda (variable init) (list 'set! variable init))
                         (let-variables exp)
                         (let-inits exp))
                    (let-body exp))))

(define eval-without-letrec mc-eval)

(define (mc-eval exp env)
  (if (letrec? exp)
      (mc-eval (letrec->let exp) env)
      (eval-without-letrec exp env)))

(check (letrec->let '(letrec ((f (lambda () (g))) (g (lambda () 1))) (f)))
       => '(let ((f '*unassigned*) (g '*unassigned*))
             (set! f (lambda () (g)))
             (set! g (lambda () 1))
             (f)))

(define (parity-program binding-form)
  `(define (f x)
     (,binding-form
      ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
       (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
      (list (even? x) (odd? x)))))

(check (interpret (parity-program 'letrec) '(f 5)) => '(#f #t))
(check (interpret '(letrec ((fact (lambda (n)
                                    (if (= n 1) 1 (* n (fact (- n 1)))))))
                     (fact 10)))
       => 3628800)
(check (interpret '(letrec () 1)) => 1)
(check-error (interpret '(letrec ((a (+ a 1))) a)))

;;; b.
;; With letrec, the frame binding even? and odd? is created first, and the
;; lambdas are evaluated in it, so both procedures' environments point to
;; that frame and each can find the other.
;;
;; With let, the lambdas are operands of the combination the let stands for,
;; so they are evaluated in the frame of the call to f, before the new frame
;; exists. Their environments point to f's frame, where even? and odd? are
;; not bound: the new frame is only used by the body of the let. Calling
;; even? or odd? on a non-zero argument looks up the other procedure in f's
;; frame and in the global environment, and fails.

(check (interpret (parity-program 'let) '(f 0)) => '(#t #f))
(check-error (interpret (parity-program 'let) '(f 5)))
