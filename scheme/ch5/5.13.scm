(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (name) ((machine 'allocate-register) name))
              (controller-registers controller-text))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (controller-registers controller-text)
  (delete-duplicates
   (append-map instruction-registers
               (remove symbol? controller-text))))

(define (instruction-registers inst)
  (case (car inst)
    ((assign)
     (cons (assign-reg-name inst)
           (expression-registers (assign-value-exp inst))))
    ((save restore) (list (stack-inst-reg-name inst)))
    (else (expression-registers (cdr inst)))))

(define (expression-registers exps)
  (filter-map (lambda (exp) (and (register-exp? exp) (register-exp-reg exp)))
              exps))

(check (controller-registers gcd-controller) => '(b t a))
(check (controller-registers factorial-controller) => '(continue n val))
(check (controller-registers fib-controller) => '(continue n val))
(check (controller-registers
        '((perform (op print) (reg x))
          (goto (reg k))))
       => '(x k))

(check (run-machine (make-machine arithmetic-operations gcd-controller)
                    '((a 206) (b 40))
                    'a)
       => 2)
(check (run-machine (make-machine arithmetic-operations factorial-controller)
                    '((n 5))
                    'val)
       => 120)
(check (run-machine (make-machine arithmetic-operations fib-controller)
                    '((n 10))
                    'val)
       => 55)

(check-error (get-register (make-machine '() '((assign a (const 1)))) 'b))
