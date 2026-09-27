(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "5.52/compile-to-c.scm" (current-load-pathname)))
(load-option 'synchronous-subprocess)

;; Exercise 5.52: compile-to-c.scm turns Scheme programs into C, which is
;; linked with runtime.c and the data layer of exercise 5.51. Compiled
;; programs print only what they display; an error ends them with status 1.

(define (run-command command input)
  (let ((output (open-output-string)))
    (let ((status (run-shell-command command
                                     'input (open-input-string input)
                                     'output output)))
      (cons status (get-output-string output)))))

(define (lines . strings)
  (apply string-append
         (map (lambda (string) (string-append string "\n")) strings)))

(define directory
  (->namestring (merge-pathnames "5.52/"
                                 (directory-pathname (current-load-pathname)))))

;; The builds must be free of warnings, so make prints nothing.
(check (run-command (string-append "make -s -B -j8 -C " directory " runtime")
                    "")
       => '(0 . ""))

;; Compiles the program to build/name.c and builds build/name from it.
(define (build-program name exps)
  (call-with-output-file (string-append directory "build/" name ".c")
    (lambda (port) (compile-to-c exps port)))
  (let ((result (run-command (string-append "make -s -C " directory
                                            " build/" name)
                             "")))
    (if (not (equal? result '(0 . "")))
        (error "Build failed:" name result))
    (string-append directory "build/" name)))

;; Returns the exit status and the output of the compiled program.
(define (run-program exps)
  (run-command (build-program "program" exps) ""))

(define (program-output exps)
  (cdr (run-program exps)))

;;; The C code for a call of a primitive procedure

(define (c-code exps)
  (set! label-counter 0)
  (call-with-output-string (lambda (port) (compile-to-c exps port))))

(check (c-code '((car '(a "b"))))
       => (lines "/* Compiled from Scheme by compile-to-c.scm. */"
                 ""
                 "#include \"runtime.h\""
                 ""
                 "enum {"
                 "    AFTER_CALL1,"
                 "};"
                 ""
                 "static const char *const constant_texts[] = {"
                 "    \"car\","
                 "    \"(a \\\"b\\\")\","
                 "};"
                 ""
                 "static Value constants[2];"
                 ""
                 "void run_program(void)"
                 "{"
                 "    int target;"
                 "    load_constants(constants, constant_texts, 2);"
                 ""
                 "    reg.proc = lookup_variable_value(constants[0], reg.env);"
                 "    reg.val = constants[1];"
                 "    reg.argl = list1(reg.val);"
                 "    if (is_primitive(reg.proc)) goto primitive_branch3;"
                 "    reg.cont = make_label(AFTER_CALL1);"
                 "    reg.val = compiled_procedure_entry(reg.proc);"
                 "    target = label_of(reg.val);"
                 "    goto dispatch;"
                 "primitive_branch3:"
                 "    reg.val = apply_primitive_procedure(reg.proc, reg.argl);"
                 "after_call1:"
                 "    return;"
                 ""
                 "dispatch:"
                 "    switch (target) {"
                 "    case AFTER_CALL1: goto after_call1;"
                 "    }"
                 "}"))

(check (program-output '((display (car '(a "b"))))) => "a")

;;; Compiled programs

(check (program-output
        '((define (factorial n)
            (if (= n 0) 1 (* n (factorial (- n 1)))))
          (display (factorial 20))
          (newline)))
       => (lines "2432902008176640000"))

(check (program-output
        '((define (make-counter)
            (let ((count 0))
              (lambda () (set! count (+ count 1)) count)))
          (define counter (make-counter))
          (counter)
          (display (counter))
          (define (sign x)
            (cond ((< x 0) 'negative) ((= x 0) 'zero) (else 'positive)))
          (display (list (sign -1.5) (sign 0) (sign 2)))
          (display (let loop ((i 0) (acc '()))
                     (if (= i 3) acc (loop (+ i 1) (cons i acc)))))
          (display (list (and) (and 1 2) (and #f (car '()))
                         (or) (or #f 2) (or 1 (car '()))))
          (display '("s" 2.5 (a . b)))))
       => "2(negative zero positive)(2 1 0)(#t 2 #f #f 2 1)(s 2.5 (a . b))")

;; Like the book's compiler, the C code evaluates operands right to left.
(check (program-output '((define n 0)
                         (define (next!) (set! n (+ n 1)) n)
                         (display (list (next!) (next!)))))
       => "(2 1)")

;; The loop runs in constant space, while count needs stack space for each
;; of its 10^6 pending additions, more than the stack's 10^5 places.
(check (run-program
        '((define (loop n) (if (= n 0) 'done (loop (- n 1))))
          (display (loop 1000000))
          (newline)
          (define (count n) (if (= n 0) 0 (+ 1 (count (- n 1)))))
          (display (count 1000000))))
       => (cons 1 (lines "done"
                         ";Aborting!: maximum recursion depth exceeded")))

;; The memory holds 2^18 pairs, so churn needs many collections, and keep
;; has to survive them.
(check (program-output
        '((define (iota n) (if (= n 0) '() (cons n (iota (- n 1)))))
          (define (sum list)
            (if (null? list) 0 (+ (car list) (sum (cdr list)))))
          (define keep (iota 1000))
          (define (churn k)
            (if (> k 0) (begin (iota 1000) (churn (- k 1)))))
          (churn 1000)
          (display (sum keep))))
       => "500500")

(check (run-program '((display "before ") (car 1) (display "after")))
       => (cons 1 (lines "before ;The object passed to car is not a pair: 1")))

;;; The metacircular evaluator compiled to C

(define (read-forms filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((forms '()))
        (let ((form (read port)))
          (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms))))))))

;; The evaluator of ch4/lib/mceval.scm needs map, which cannot be a
;; primitive because it calls a procedure, and a driver loop that stops at
;; the end of the input.
(define metacircular-evaluator
  (append
   '((define (map procedure list)
       (if (null? list)
           '()
           (cons (procedure (car list)) (map procedure (cdr list))))))
   (read-forms (merge-pathnames "../ch4/lib/mceval.scm" (current-load-pathname)))
   '((define (evaluate-input)
       (let ((input (read)))
         (if (not (eof-object? input))
             (begin
               (user-print (mc-eval input the-global-environment))
               (newline)
               (evaluate-input)))))
     (evaluate-input))))

(define mc-evaluator (build-program "mceval" metacircular-evaluator))

(define (mc-eval-output program)
  (cdr (run-command mc-evaluator program)))

(check (mc-eval-output
        "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
         (factorial 10)
         (define (make-account balance)
           (lambda (amount)
             (if (> amount balance)
                 \"Insufficient funds\"
                 (begin (set! balance (- balance amount)) balance))))
         (define withdraw (make-account 100))
         (withdraw 30)
         (withdraw 80)
         (cond ((null? '(1)) 'empty) ((pair? '(1)) 'pair) (else 'other))
         (define (loop n) (if (= n 0) 'done (loop (- n 1))))
         (loop 1000)
         (lambda (x) (* x x))")
       => (lines "ok" "3628800" "ok" "ok" "70" "Insufficient funds" "pair"
                 "ok" "done"
                 "(compound-procedure (x) ((* x x)) <procedure-env>)"))

;; Errors of the interpreted program are errors of the compiled evaluator,
;; which reports them with the error primitive of the runtime.
(check (run-command mc-evaluator "(car '(a b)) (undefined-variable) 'never")
       => (cons 1 (lines "a" ";Unbound variable undefined-variable")))
