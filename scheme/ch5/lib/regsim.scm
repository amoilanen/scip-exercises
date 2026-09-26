;; Register-machine simulator (section 5.2).
;; Keeps the book's interface and procedure names so that exercises can load
;; this file and redefine individual pieces.

;;; Registers

(define (make-register name)
  (let ((contents '*unassigned*))
    (lambda (message)
      (case message
        ((get) contents)
        ((set) (lambda (value) (set! contents value)))
        (else (error "Unknown request -- REGISTER" message))))))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

;;; Stack, monitored as in section 5.2.4

(define (make-stack)
  (let ((items '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! items (cons x items))
      (set! number-pushes (+ number-pushes 1))
      (set! current-depth (+ current-depth 1))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? items)
          (error "Empty stack -- POP")
          (let ((top (car items)))
            (set! items (cdr items))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! items '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (statistics)
      (list (cons 'total-pushes number-pushes)
            (cons 'maximum-depth max-depth)))
    (lambda (message)
      (case message
        ((push) push)
        ((pop) (pop))
        ((initialize) (initialize))
        ((statistics) (statistics))
        ((print-statistics)
         (newline)
         (display (statistics)))
        (else (error "Unknown request -- STACK" message))))))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;;; Machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (instruction-sequence '()))
    (let ((operations
           (list (list 'initialize-stack (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register:" name)
            (set! register-table
                  (cons (list name (make-register name)) register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((entry (assoc name register-table)))
          (if entry
              (cadr entry)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (case message
          ((start)
           (set-contents! pc instruction-sequence)
           (execute))
          ((install-instruction-sequence)
           (lambda (seq) (set! instruction-sequence seq)))
          ((allocate-register) allocate-register)
          ((get-register) lookup-register)
          ((install-operations)
           (lambda (ops) (set! operations (append operations ops))))
          ((stack) stack)
          ((operations) operations)
          (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (name) ((machine 'allocate-register) name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (start machine) (machine 'start))

(define (stack-statistics machine)
  ((machine 'stack) 'statistics))

(define (get-register machine register-name)
  ((machine 'get-register) register-name))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

;;; Assembler

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst insts) labels))
               (receive (cons (make-instruction next-inst) insts)
                        labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc) (set-cdr! inst proc))

(define (make-label-entry label-name insts) (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((entry (assoc label-name labels)))
    (if entry
        (cdr entry)
        (error "Undefined label -- ASSEMBLE" label-name))))

;;; Execution procedures

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (case (car inst)
    ((assign) (make-assign inst machine labels ops pc))
    ((test) (make-test inst machine labels ops flag pc))
    ((branch) (make-branch inst machine labels flag pc))
    ((goto) (make-goto inst machine labels pc))
    ((save) (make-save inst machine stack pc))
    ((restore) (make-restore inst machine stack pc))
    ((perform) (make-perform inst machine labels ops pc))
    (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (register-exp-or-operation exp machine labels ops)
  (if (operation-exp? exp)
      (make-operation-exp exp machine labels ops)
      (make-primitive-exp (car exp) machine labels)))

(define (make-assign inst machine labels ops pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-proc (register-exp-or-operation (assign-value-exp inst)
                                               machine labels ops)))
    (lambda ()
      (set-contents! target (value-proc))
      (advance-pc pc))))

(define (assign-reg-name inst) (cadr inst))
(define (assign-value-exp inst) (cddr inst))

(define (make-test inst machine labels ops flag pc)
  (let ((condition (test-condition inst)))
    (if (not (operation-exp? condition))
        (error "Bad TEST instruction -- ASSEMBLE" inst))
    (let ((condition-proc (make-operation-exp condition machine labels ops)))
      (lambda ()
        (set-contents! flag (condition-proc))
        (advance-pc pc)))))

(define (test-condition inst) (cdr inst))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (not (label-exp? dest))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))
    (let ((insts (lookup-label labels (label-exp-label dest))))
      (lambda ()
        (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc))))))

(define (branch-dest inst) (cadr inst))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest inst) (cadr inst))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name inst) (cadr inst))

(define (make-perform inst machine labels ops pc)
  (let ((action (perform-action inst)))
    (if (not (operation-exp? action))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))
    (let ((action-proc (make-operation-exp action machine labels ops)))
      (lambda ()
        (action-proc)
        (advance-pc pc)))))

(define (perform-action inst) (cdr inst))

;;; Subexpressions

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((value (constant-exp-value exp)))
           (lambda () value)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((reg (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents reg))))
        (else (error "Unknown expression type -- ASSEMBLE" exp))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels ops)
  (let ((op (lookup-prim (operation-exp-op exp) ops))
        (arg-procs
         (map (lambda (operand) (make-primitive-exp operand machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (arg-proc) (arg-proc)) arg-procs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op exp) (cadr (car exp)))
(define (operation-exp-operands exp) (cdr exp))

(define (lookup-prim symbol ops)
  (let ((entry (assoc symbol ops)))
    (if entry
        (cadr entry)
        (error "Unknown operation -- ASSEMBLE" symbol))))
