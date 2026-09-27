;; The explicit-control evaluator extended to work with compiled code
;; (section 5.5.7): interpreted code can apply compiled procedures, and
;; compiled code is run by entering the evaluator at external-entry.
;; The compapp register holds the entry point compiled code uses to call
;; interpreted procedures (exercise 5.47).
;;
;;   (compile-and-go machine exp)     runs compiled exp in a fresh global
;;                                    environment and returns its value
;;   (eceval-execute-compiled machine exp)
;;   (eceval-interpret machine exp)   run exp in the current global
;;                                    environment, compiled or interpreted
;;   (make-compiled-eceval dispatch-table extra-code extra-operations)

(load (merge-pathnames "compiler.scm" (current-load-pathname)))
(load (merge-pathnames "eceval.scm" (current-load-pathname)))

(define compiled-apply-dispatch-code
  (append '(apply-dispatch
            (test (op compiled-procedure?) (reg proc))
            (branch (label compiled-apply)))
          (cdr apply-dispatch-code)
          '(compiled-apply
            (restore continue)
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))

;; The flag register chooses between interpreting exp and running the
;; compiled code in val.
(define external-entry-code
  '(external-entry
    (perform (op initialize-stack))
    (assign continue (label done))
    (goto (reg val))))

(define (make-compiled-eceval dispatch-table extra-code extra-operations)
  (make-machine
   (list-union (cons 'compapp eceval-registers) all-regs)
   (append eceval-operations
           compiled-code-operations
           (list (list 'compiled-procedure? compiled-procedure?))
           extra-operations)
   (fluid-let ((apply-dispatch-code compiled-apply-dispatch-code))
     (append '((assign compapp (label compound-apply))
               (branch (label external-entry)))
             (eceval-controller dispatch-table
                                (append external-entry-code extra-code))))))

(define compiled-eceval (make-compiled-eceval eceval-dispatch-table '() '()))

(define (run-compiled-eceval machine compiled-entry?)
  (set-register-contents! machine 'env the-global-environment)
  (set-register-contents! machine 'flag compiled-entry?)
  (start machine)
  (get-register-contents machine 'val))

(define (eceval-interpret machine exp)
  (set-register-contents! machine 'exp exp)
  (run-compiled-eceval machine false))

(define (assemble-compiled exp machine)
  (assemble (statements (compile exp 'val 'return)) machine))

(define (eceval-execute-compiled machine exp)
  (set-register-contents! machine 'val (assemble-compiled exp machine))
  (run-compiled-eceval machine true))

(define (compile-and-go machine exp)
  (set! the-global-environment (setup-environment))
  (eceval-execute-compiled machine exp))
