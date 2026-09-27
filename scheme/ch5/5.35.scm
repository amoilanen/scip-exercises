(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/compiler.scm" (current-load-pathname)))

;; The code of figure 5.18 defines f with one parameter x.  Its body looks
;; up + and evaluates the operands right to left: first (g (+ x 2)), whose
;; value becomes the last argument, and then x.  So it was compiled from

(define decompiled '(define (f x) (+ x (g (+ x 2)))))

(define figure-5.18
  '((assign val (op make-compiled-procedure) (label entry16) (reg env))
    (goto (label after-lambda15))
    entry16
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (save continue)
    (save proc)
    (save env)
    (assign proc (op lookup-variable-value) (const g) (reg env))
    (save proc)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (assign val (const 2))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const x) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch19))
    compiled-branch18
    (assign continue (label after-call17))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch19
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call17
    (assign argl (op list) (reg val))
    (restore proc)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch22))
    compiled-branch21
    (assign continue (label after-call20))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch22
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call20
    (assign argl (op list) (reg val))
    (restore env)
    (assign val (op lookup-variable-value) (const x) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)
    (restore continue)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch25))
    compiled-branch24
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    primitive-branch25
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    after-call23
    after-lambda15
    (perform (op define-variable!) (const f) (reg val) (reg env))
    (assign val (const ok))))

;; Labels are numbered by a global counter, so code is compared after
;; numbering the labels in order of their appearance.
(define (renumber-labels code)
  (let ((numbers '()))
    (define (number-of label)
      (let ((entry (assq label numbers)))
        (if entry
            (cdr entry)
            (let ((number (length numbers)))
              (set! numbers (cons (cons label number) numbers))
              number))))
    (define (renumber exp)
      (cond ((label-exp? exp) (list 'label (number-of (label-exp-label exp))))
            ((pair? exp) (map renumber exp))
            (else exp)))
    (map (lambda (item)
           (if (symbol? item) (number-of item) (renumber item)))
         code)))

(check (renumber-labels (statements (compile decompiled 'val 'next)))
       => (renumber-labels figure-5.18))

(check (compile-and-run decompiled '(define (g y) (* y 10)) '(f 1)) => 31)
