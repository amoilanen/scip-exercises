(load "lib/check.scm")
(load "ch5/lib/regsim.scm")
(load "ch5/lib/machines.scm")

;; A terser syntax:
;;   registers are bare symbols:          (assign a b), (goto continue)
;;   constants are literals or quoted:    (assign n 1), (assign x '(a b))
;;   labels are still marked:             (branch (label done))
;;   operations are written as calls:     (assign t (rem a b)), (test (= b 0))
;; Only the syntax procedures change; the rest of the simulator is untouched.

(define (register-exp? exp) (symbol? exp))
(define (register-exp-reg exp) exp)

(define (constant-exp? exp)
  (or (number? exp) (string? exp) (boolean? exp) (char? exp)
      (tagged-list? exp 'quote)))

(define (constant-exp-value exp)
  (if (pair? exp) (cadr exp) exp))

(define (operation-exp? exp)
  (and (pair? exp)
       (not (label-exp? exp))
       (not (constant-exp? exp))))

(define (operation-exp-op exp) (car exp))
(define (operation-exp-operands exp) (cdr exp))

(define (assign-value-exp inst) (caddr inst))
(define (test-condition inst) (cadr inst))
(define (perform-action inst) (cadr inst))

(define (register-exp-or-operation exp machine labels ops)
  (if (operation-exp? exp)
      (make-operation-exp exp machine labels ops)
      (make-primitive-exp exp machine labels)))

(define terse-gcd-controller
  '(test-b
      (test (= b 0))
      (branch (label gcd-done))
      (assign t (rem a b))
      (assign a b)
      (assign b t)
      (goto (label test-b))
    gcd-done))

(define terse-factorial-controller
  '((assign continue (label fact-done))
    fact-loop
      (test (= n 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (- n 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
    after-fact
      (restore n)
      (restore continue)
      (assign val (* n val))
      (goto continue)
    base-case
      (assign val 1)
      (goto continue)
    fact-done))

(check (run-machine (make-machine '(a b t)
                                  arithmetic-operations
                                  terse-gcd-controller)
                    '((a 206) (b 40))
                    'a)
       => 2)

(check (run-machine (make-machine '(n val continue)
                                  arithmetic-operations
                                  terse-factorial-controller)
                    '((n 6))
                    'val)
       => 720)

(define countdown-controller
  '((assign result '())
    loop
      (test (= n 0))
      (branch (label done))
      (assign result (cons n result))
      (assign n (- n 1))
      (goto (label loop))
    done
      (assign result (cons "go" result))
      (assign result (cons 'ready result))))

(check (run-machine (make-machine '(n result)
                                  (cons (list 'cons cons)
                                        arithmetic-operations)
                                  countdown-controller)
                    '((n 3))
                    'result)
       => '(ready "go" 1 2 3))

;; The old syntax is no longer understood: (reg b) reads as an operation.
(check-error (make-machine '(a b) '() '((assign a (reg b)))))
