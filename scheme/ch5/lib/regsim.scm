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

;;; Stack

(define (make-stack)
  (let ((items '()))
    (lambda (message)
      (case message
        ((push) (lambda (x) (set! items (cons x items))))
        ((pop)
         (if (null? items)
             (error "Empty stack -- POP")
             (let ((top (car items)))
               (set! items (cdr items))
               top)))
        ((initialize) (set! items '()) 'done)
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
           (list (list 'initialize-stack (lambda () (stack 'initialize)))))
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
