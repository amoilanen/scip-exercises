(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/regsim.scm" (current-load-pathname)))
(load (merge-pathnames "lib/machines.scm" (current-load-pathname)))

;; The information is derived from the texts of the assembled instructions.
;; The new machine keeps them and delegates all other requests to the
;; original one.

(define instruction-types '(assign test branch goto save restore perform))

(define (instructions-of-type type texts)
  (filter (lambda (text) (eq? (car text) type)) texts))

(define (sorted-instructions texts)
  (append-map (lambda (type)
                (delete-duplicates (instructions-of-type type texts)))
              instruction-types))

(define (entry-point-registers texts)
  (delete-duplicates
   (filter-map (lambda (goto)
                 (let ((dest (goto-dest goto)))
                   (and (register-exp? dest) (register-exp-reg dest))))
               (instructions-of-type 'goto texts))))

(define (stack-registers texts)
  (delete-duplicates
   (map stack-inst-reg-name
        (append (instructions-of-type 'save texts)
                (instructions-of-type 'restore texts)))))

(define (register-sources texts)
  (let ((assigns (instructions-of-type 'assign texts)))
    (map (lambda (reg)
           (cons reg
                 (delete-duplicates
                  (filter-map (lambda (assign)
                                (and (eq? (assign-reg-name assign) reg)
                                     (assign-value-exp assign)))
                              assigns))))
         (delete-duplicates (map assign-reg-name assigns)))))

(define make-machine-without-info make-new-machine)

(define (make-new-machine)
  (let ((machine (make-machine-without-info))
        (texts '()))
    (lambda (message)
      (case message
        ((install-instruction-sequence)
         (lambda (seq)
           (set! texts (map instruction-text seq))
           ((machine 'install-instruction-sequence) seq)))
        ((instructions) (sorted-instructions texts))
        ((entry-point-registers) (entry-point-registers texts))
        ((stack-registers) (stack-registers texts))
        ((register-sources) (register-sources texts))
        (else (machine message))))))

(define fib-machine (make-fib-machine))

(check (fib-machine 'instructions)
       => '((assign continue (label fib-done))
            (assign continue (label afterfib-n-1))
            (assign n (op -) (reg n) (const 1))
            (assign n (op -) (reg n) (const 2))
            (assign continue (label afterfib-n-2))
            (assign n (reg val))
            (assign val (op +) (reg val) (reg n))
            (assign val (reg n))
            (test (op <) (reg n) (const 2))
            (branch (label immediate-answer))
            (goto (label fib-loop))
            (goto (reg continue))
            (save continue)
            (save n)
            (save val)
            (restore n)
            (restore continue)
            (restore val)))

(check (fib-machine 'entry-point-registers) => '(continue))
(check (fib-machine 'stack-registers) => '(continue n val))
(check (fib-machine 'register-sources)
       => '((continue ((label fib-done))
                      ((label afterfib-n-1))
                      ((label afterfib-n-2)))
            (n ((op -) (reg n) (const 1))
               ((op -) (reg n) (const 2))
               ((reg val)))
            (val ((op +) (reg val) (reg n))
                 ((reg n)))))

(check (run-machine fib-machine '((n 10)) 'val) => 55)

(define gcd-machine (make-gcd-machine))
(check (gcd-machine 'entry-point-registers) => '())
(check (gcd-machine 'stack-registers) => '())
(check (gcd-machine 'register-sources)
       => '((t ((op rem) (reg a) (reg b)))
            (a ((reg b)))
            (b ((reg t)))))
