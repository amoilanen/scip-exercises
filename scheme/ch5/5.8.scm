(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))

(define ambiguous-controller
  '(start
      (goto (label here))
    here
      (assign a (const 3))
      (goto (label there))
    here
      (assign a (const 4))
      (goto (label there))
    there))

(define (run-ambiguous-controller)
  (let ((machine (make-machine '(a) '() ambiguous-controller)))
    (start machine)
    (get-register-contents machine 'a)))

;; extract-labels builds the label list from the end of the controller
;; backwards, so the first here ends up in front of the second one, and
;; lookup-label (via assoc) finds it: a gets 3.
(check (run-ambiguous-controller) => 3)

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (cond ((not (symbol? next-inst))
                  (receive (cons (make-instruction next-inst) insts) labels))
                 ((assoc next-inst labels)
                  (error "Multiply defined label -- ASSEMBLE" next-inst))
                 (else
                  (receive insts
                           (cons (make-label-entry next-inst insts)
                                 labels)))))))))

(check-error (run-ambiguous-controller))

(define unambiguous-controller
  '(start
      (goto (label there))
    here
      (assign a (const 3))
    there
      (assign a (const 4))))

(let ((machine (make-machine '(a) '() unambiguous-controller)))
  (start machine)
  (check (get-register-contents machine 'a) => 4))
