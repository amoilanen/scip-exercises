;; The compiler of section 5.5, plus a helper that runs compiled code on the
;; register-machine simulator.
;;
;;   (compile exp target linkage)  returns an instruction sequence
;;   (compile-and-run exp ...)     compiles and runs expressions, returns val

(load (merge-pathnames "../../ch4/lib/mceval.scm" (current-load-pathname)))
(load (merge-pathnames "regsim.scm" (current-load-pathname)))

;;; Instruction sequences

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg) (memq reg (registers-needed seq)))
(define (modifies-register? seq reg) (memq reg (registers-modified seq)))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

;; Sequential composition: a register is needed by the combination if the
;; first sequence needs it, or the second needs it and the first doesn't set it.
(define (append-instruction-sequences . seqs)
  (define (append-2 seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (fold-right append-2 (empty-instruction-sequence) seqs))

;; Like append, but saves and restores each register in regs that seq1
;; modifies and seq2 needs.
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((reg (car regs)))
        (if (and (needs-register? seq2 reg)
                 (modifies-register? seq1 reg))
            (preserving
             (cdr regs)
             (make-instruction-sequence
              (list-union (list reg) (registers-needed seq1))
              (list-difference (registers-modified seq1) (list reg))
              (append `((save ,reg))
                      (statements seq1)
                      `((restore ,reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

;; Appends body-seq, which is never executed by falling through, e.g. the
;; code of a lambda.
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

;; Combines the two alternative branches of a test.
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1) (registers-needed seq2))
   (list-union (registers-modified seq1) (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;;; Labels

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ label-counter 1))
  label-counter)

(define (make-label name)
  (symbol-append name (string->symbol (number->string (new-label-number)))))

(define all-regs '(env proc val argl continue))

;;; Compiling expressions

(define (compile exp target linkage)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp) (compile-variable exp target linkage))
        ((assignment? exp) (compile-assignment exp target linkage))
        ((definition? exp) (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp) (compile-application exp target linkage))
        (else (error "Unknown expression type -- COMPILE" exp))))

;; linkage is one of: next, return, or a label to jump to.
(define (compile-linkage linkage)
  (case linkage
    ((return)
     (make-instruction-sequence '(continue) '() '((goto (reg continue)))))
    ((next) (empty-instruction-sequence))
    (else
     (make-instruction-sequence '() '() `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-constant value target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,value))))))

(define (compile-self-evaluating exp target linkage)
  (compile-constant exp target linkage))

(define (compile-quoted exp target linkage)
  (compile-constant (text-of-quotation exp) target linkage))

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) (list target)
    `((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))))

;; Assignments and definitions share everything but the operation that
;; stores the value.
(define (compile-variable-store operation variable value-exp target linkage)
  (let ((get-value-code (compile value-exp 'val 'next)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op ,operation) (const ,variable) (reg val) (reg env))
         (assign ,target (const ok))))))))

(define (compile-assignment exp target linkage)
  (compile-variable-store 'set-variable-value!
                          (assignment-variable exp)
                          (assignment-value exp)
                          target linkage))

(define (compile-definition exp target linkage)
  (compile-variable-store 'define-variable!
                          (definition-variable exp)
                          (definition-value exp)
                          target linkage))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    ;; The true branch cannot fall through into the false branch.
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code (compile (if-consequent exp) target consequent-linkage))
            (a-code (compile (if-alternative exp) target linkage)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))

;;; Compiled procedures

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

;;; Applications

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

;; Operands are evaluated right to left so that argl can be built with cons.
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;; Calls the compiled procedure in proc. When the target is not val, the
;; procedure returns to proc-return, where its value is moved to the target.
(define (compile-proc-appl target linkage)
  (define (call-with-return-to return-label)
    `((assign continue (label ,return-label))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))))
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    (call-with-return-to linkage)))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            (append (call-with-return-to proc-return)
                    `(,proc-return
                      (assign ,target (reg val))
                      (goto (label ,linkage)))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        (else
         (error "return linkage, target not val -- COMPILE" target))))

;;; Running compiled code

(define compiled-code-operations
  (list (list 'lookup-variable-value lookup-variable-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'define-variable! define-variable!)
        (list 'extend-environment extend-environment)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'false? false?)
        (list 'list list)
        (list 'cons cons)))

;; A machine that runs the compiled expressions in a fresh global environment.
(define (make-compiled-machine . exps)
  (let ((machine
         (make-machine all-regs
                       compiled-code-operations
                       (statements
                        (compile (make-begin exps) 'val 'next)))))
    (set-register-contents! machine 'env (setup-environment))
    machine))

(define (compile-and-run . exps)
  (let ((machine (apply make-compiled-machine exps)))
    (start machine)
    (get-register-contents machine 'val)))
