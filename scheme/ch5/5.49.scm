(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/compiler.scm" (current-load-pathname)))

;; Every expression read is compiled, assembled into the machine and run;
;; the compiled code returns to print-result through continue.  The loop
;; stops at the end of the input.

(define rcepl-input-prompt ";;; RCEPL input:")
(define rcepl-output-prompt ";;; RCEPL value:")

(define rcepl-controller
  `(read-compile-execute-print-loop
    (perform (op initialize-stack))
    (perform (op prompt-for-input) (const ,rcepl-input-prompt))
    (assign val (op read))
    (test (op eof-object?) (reg val))
    (branch (label done))
    (assign val (op compile-and-assemble) (reg val))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (reg val))
    print-result
    (perform (op announce-output) (const ,rcepl-output-prompt))
    (perform (op user-print) (reg val))
    (goto (label read-compile-execute-print-loop))
    done))

(define (compile-and-assemble exp)
  (assemble (statements (compile exp 'val 'return)) rcepl))

(define (get-global-environment) the-global-environment)

(define (rcepl-print object)
  (if (compiled-procedure? object)
      (display '<compiled-procedure>)
      (display object)))

(define rcepl
  (make-machine
   all-regs
   (append (list (list 'read read)
                 (list 'eof-object? eof-object?)
                 (list 'compile-and-assemble compile-and-assemble)
                 (list 'get-global-environment get-global-environment)
                 (list 'prompt-for-input prompt-for-input)
                 (list 'announce-output announce-output)
                 (list 'user-print rcepl-print))
           compiled-code-operations)
   rcepl-controller))

(define (run-rcepl input)
  (set! the-global-environment (setup-environment))
  (with-output-to-string
    (lambda ()
      (with-input-from-string input
        (lambda () (start rcepl))))))

;; The output of a session whose inputs print the given values.
(define (rcepl-transcript . printed-values)
  (let ((prompt (string-append "\n\n" rcepl-input-prompt "\n")))
    (string-append
     (apply string-append
            (map (lambda (value)
                   (string-append prompt "\n" rcepl-output-prompt "\n" value))
                 printed-values))
     prompt)))

(check (run-rcepl "") => (rcepl-transcript))
(check (run-rcepl "(+ 1 2)") => (rcepl-transcript "3"))
(check (run-rcepl "(define (factorial n)
                     (if (= n 1)
                         1
                         (* (factorial (- n 1)) n)))
                   (factorial 10)
                   factorial
                   (define answer (factorial 5))
                   (list answer 'items \"text\")")
       => (rcepl-transcript "ok"
                            "3628800"
                            "<compiled-procedure>"
                            "ok"
                            "(120 items text)"))
