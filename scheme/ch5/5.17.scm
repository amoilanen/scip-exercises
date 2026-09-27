(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "5.16.scm" (current-load-pathname)))

;; Each instruction remembers the labels that immediately precede it. Labels
;; are still not instructions: they are neither executed nor counted by the
;; instruction counter of exercise 5.15.

(define (make-instruction text) (list text '() '()))
(define (instruction-text inst) (car inst))
(define (instruction-labels inst) (cadr inst))
(define (instruction-execution-proc inst) (caddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cddr inst) proc))

(define (add-instruction-label! inst label)
  (set-car! (cdr inst) (cons label (instruction-labels inst))))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (begin
                 (if (pair? insts)
                     (add-instruction-label! (car insts) next-inst))
                 (receive insts
                          (cons (make-label-entry next-inst insts) labels)))
               (receive (cons (make-instruction next-inst) insts)
                        labels)))))))

(define (trace-instruction inst)
  (for-each (lambda (label)
              (display label)
              (display ":")
              (newline))
            (instruction-labels inst))
  (display "  ")
  (write (instruction-text inst))
  (newline))

(define factorial-machine (make-factorial-machine))
(factorial-machine 'trace-on)

(check (trace-output factorial-machine '((n 2)))
       => (lines "  (assign continue (label fact-done))"
                 "fact-loop:"
                 "  (test (op =) (reg n) (const 1))"
                 "  (branch (label base-case))"
                 "  (save continue)"
                 "  (save n)"
                 "  (assign n (op -) (reg n) (const 1))"
                 "  (assign continue (label after-fact))"
                 "  (goto (label fact-loop))"
                 "fact-loop:"
                 "  (test (op =) (reg n) (const 1))"
                 "  (branch (label base-case))"
                 "base-case:"
                 "  (assign val (const 1))"
                 "  (goto (reg continue))"
                 "after-fact:"
                 "  (restore n)"
                 "  (restore continue)"
                 "  (assign val (op *) (reg n) (reg val))"
                 "  (goto (reg continue))"))
(check (get-register-contents factorial-machine 'val) => 2)

;; The trace shows as many instructions as exercise 5.15 counted: 11n - 6.
(define (traced-instruction-count n)
  (let ((trace (trace-output factorial-machine (list (list 'n n)))))
    (length (filter (lambda (line) (string-prefix? "  " line))
                    ((string-splitter 'delimiter #\newline) trace)))))

(check (traced-instruction-count 5) => 49)

(define consecutive-labels-machine
  (make-machine '(val) '()
                '(start
                    (goto (label second))
                  first
                  second
                    (assign val (const 1))
                  done)))
(consecutive-labels-machine 'trace-on)

(check (trace-output consecutive-labels-machine '())
       => (lines "start:"
                 "  (goto (label second))"
                 "first:"
                 "second:"
                 "  (assign val (const 1))"))
