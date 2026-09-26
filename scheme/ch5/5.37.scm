(load "lib/check.scm")
(load "ch5/lib/compiler.scm")

(define optimizing-preserving preserving)

(define (saving-preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((reg (car regs)))
        (saving-preserving
         (cdr regs)
         (make-instruction-sequence
          (list-union (list reg) (registers-needed seq1))
          (list-difference (registers-modified seq1) (list reg))
          (append `((save ,reg))
                  (statements seq1)
                  `((restore ,reg))))
         seq2))))

(define (compile-with preserving-strategy exp)
  (fluid-let ((preserving preserving-strategy))
    (statements (compile exp 'val 'next))))

(define (saved-registers code)
  (filter-map (lambda (inst) (and (tagged-list? inst 'save) (cadr inst)))
              code))

;; A variable reference: continue is saved around the lookup, although the
;; lookup doesn't change it, and nothing follows that would need it.
(check (compile-with saving-preserving 'x)
       => '((save continue)
            (assign val (op lookup-variable-value) (const x) (reg env))
            (restore continue)))
(check (compile-with optimizing-preserving 'x)
       => '((assign val (op lookup-variable-value) (const x) (reg env))))

;; (f 'x 'y): continue is saved around every variable lookup and constant
;; (by end-with-linkage), around the primitive application, and together with
;; env around the operator and proc around the operands; env is saved around
;; the first operand evaluated and argl around the second.  None of these is
;; needed: variables and constants modify nothing but their target.
(check (saved-registers (compile-with saving-preserving '(f 'x 'y)))
       => '(continue env continue
            continue proc env continue argl continue
            continue))
(check (saved-registers (compile-with optimizing-preserving '(f 'x 'y)))
       => '())

;; Even when some saves are needed, most of them are not.
(define factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(check (length (saved-registers (compile-with saving-preserving factorial)))
       => 41)
(check (saved-registers (compile-with optimizing-preserving factorial))
       => '(continue env continue proc argl proc))

;; The extra saves make the code slower but not wrong.
(define (run-factorial preserving-strategy)
  (fluid-let ((preserving preserving-strategy))
    (let ((machine (make-compiled-machine factorial '(factorial 6))))
      (start machine)
      (list (get-register-contents machine 'val)
            (cdr (assq 'total-pushes (stack-statistics machine)))))))

(check (run-factorial optimizing-preserving) => '(720 32))
(check (run-factorial saving-preserving) => '(720 204))
