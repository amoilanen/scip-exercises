;; Exercise 5.52: the compiler of section 5.5 with a back end that writes C.
;;
;; The front end is the book's compiler, extended with let, named let, and,
;; or and boolean constants. Each instruction of the register-machine code
;; it produces becomes a C statement: registers are fields of reg, labels
;; are C labels, and a goto to a label held in a register jumps through a
;; switch on the label's number. runtime.c supplies the operations, and
;; the data layer (object.c, memory.c and the rest) the objects and
;; garbage collection.
;;
;;   (compile-to-c exps port)  writes a C program that evaluates exps

(load (merge-pathnames "../lib/compiler.scm" (current-load-pathname)))

;;; Derived expressions

(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (and (let? exp) (symbol? (cadr exp))))

;; (let name ((v e) ...) body) is (((lambda () (define (name v ...) body)
;; name)) e ...), so the body sees name but the e's do not.
(define (let->combination exp)
  (let* ((named? (named-let? exp))
         (bindings (if named? (caddr exp) (cadr exp)))
         (body (if named? (cdddr exp) (cddr exp)))
         (variables (map car bindings))
         (operands (map cadr bindings)))
    (if named?
        (let ((name (cadr exp)))
          (cons (list (make-lambda
                       '()
                       (list (cons 'define (cons (cons name variables) body))
                             name)))
                operands))
        (cons (make-lambda variables body) operands))))

(define (and->if exps)
  (cond ((null? exps) #t)
        ((null? (cdr exps)) (car exps))
        (else (make-if (car exps) (and->if (cdr exps)) #f))))

;; The rest of the or is wrapped in a procedure, so that the variables
;; value and rest cannot capture variables of the program.
(define (or->combination exps)
  (cond ((null? exps) #f)
        ((null? (cdr exps)) (car exps))
        (else
         `((lambda (value rest) (if value value (rest)))
           ,(car exps)
           (lambda () ,(or->combination (cdr exps)))))))

(define book-compile compile)

(define (compile exp target linkage)
  (cond ((boolean? exp) (compile-constant exp target linkage))
        ((let? exp) (compile (let->combination exp) target linkage))
        ((tagged-list? exp 'and)
         (compile (and->if (cdr exp)) target linkage))
        ((tagged-list? exp 'or)
         (compile (or->combination (cdr exp)) target linkage))
        (else (book-compile exp target linkage))))

;;; Names in C

(define (c-identifier symbol)
  (list->string
   (map (lambda (char) (if (char=? char #\-) #\_ char))
        (string->list (symbol->string symbol)))))

;; Labels that are stored in registers are numbered by an enum.
(define (label-number label)
  (string-upcase (c-identifier label)))

(define (register->c register)
  (string-append "reg."
                 (if (eq? register 'continue)
                     "cont"
                     (symbol->string register))))

(define c-operations
  '((lookup-variable-value . "lookup_variable_value")
    (set-variable-value! . "set_variable_value")
    (define-variable! . "define_variable")
    (extend-environment . "extend_environment")
    (make-compiled-procedure . "make_compiled_procedure")
    (compiled-procedure-entry . "compiled_procedure_entry")
    (compiled-procedure-env . "compiled_procedure_env")
    (primitive-procedure? . "is_primitive")
    (apply-primitive-procedure . "apply_primitive_procedure")
    (false? . "is_false")
    (list . "list1")
    (cons . "cons")))

(define (c-operation name)
  (let ((entry (assq name c-operations)))
    (if entry
        (cdr entry)
        (error "Unknown operation -- COMPILE-TO-C" name))))

(define (comma-separated strings)
  (if (null? strings)
      ""
      (fold-left (lambda (result string) (string-append result ", " string))
                 (car strings)
                 (cdr strings))))

(define (c-string-literal text)
  (define (escape char)
    (case char
      ((#\\) "\\\\")
      ((#\") "\\\"")
      ((#\newline) "\\n")
      ((#\tab) "\\t")
      (else (string char))))
  (string-append "\""
                 (apply string-append (map escape (string->list text)))
                 "\""))

;;; Constants

;; Symbols, strings and lists go into a table of constants, which the
;; program reads from their printed representations when it starts.
(define (table-constant? value)
  (or (symbol? value) (string? value) (pair? value)))

(define (datum->text datum)
  (define (tail->text tail)
    (cond ((null? tail) "")
          ((pair? tail)
           (string-append " " (datum->text (car tail))
                          (tail->text (cdr tail))))
          (else (string-append " . " (datum->text tail)))))
  (cond ((null? datum) "()")
        ((eq? datum #t) "#t")
        ((eq? datum #f) "#f")
        ((symbol? datum) (symbol->string datum))
        ((string? datum) (write-to-string datum))
        ((number? datum) (number->string datum))
        ((pair? datum)
         (string-append "(" (datum->text (car datum)) (tail->text (cdr datum))
                        ")"))
        (else (error "Unsupported constant -- COMPILE-TO-C" datum))))

;; constants maps each table constant to its index.
(define (constant->c value constants)
  (cond ((null? value) "EMPTY_LIST")
        ((eq? value #t) "TRUE_VALUE")
        ((eq? value #f) "FALSE_VALUE")
        ((and (exact-integer? value) (< (abs value) (expt 2 63)))
         (string-append "make_fixnum(" (number->string value) "L)"))
        ((and (real? value) (inexact? value) (finite? value))
         (string-append "make_flonum(" (number->string value) ")"))
        ((table-constant? value)
         (string-append "constants["
                        (number->string (hash-table-ref constants value))
                        "]"))
        (else (error "Unsupported constant -- COMPILE-TO-C" value))))

;;; Instructions

(define (operand->c operand constants)
  (case (car operand)
    ((reg) (register->c (cadr operand)))
    ((const) (constant->c (cadr operand) constants))
    ((label) (string-append "make_label(" (label-number (cadr operand)) ")"))
    (else (error "Unknown operand -- COMPILE-TO-C" operand))))

;; parts is either a single operand or an operation and its operands.
(define (value->c parts constants)
  (if (tagged-list? (car parts) 'op)
      (string-append (c-operation (cadar parts))
                     "("
                     (comma-separated
                      (map (lambda (operand) (operand->c operand constants))
                           (cdr parts)))
                     ")")
      (operand->c (car parts) constants)))

(define (goto->c destination)
  (if (eq? (car destination) 'label)
      (list (string-append "goto " (c-identifier (cadr destination)) ";"))
      (list (string-append "target = label_of("
                           (register->c (cadr destination))
                           ");")
            "goto dispatch;")))

;; Returns the C statements for an instruction other than test and branch.
(define (instruction->c instruction constants)
  (case (car instruction)
    ((assign)
     (list (string-append (register->c (cadr instruction))
                          " = "
                          (value->c (cddr instruction) constants)
                          ";")))
    ((perform)
     (list (string-append (value->c (cdr instruction) constants) ";")))
    ((goto) (goto->c (cadr instruction)))
    ((save)
     (list (string-append "save(" (register->c (cadr instruction)) ");")))
    ((restore)
     (list (string-append (register->c (cadr instruction)) " = restore();")))
    (else (error "Unknown instruction -- COMPILE-TO-C" instruction))))

;; The compiler always follows a test with a branch; together they become
;; one if statement.
(define (test-and-branch->c test branch constants)
  (if (not (and (tagged-list? branch 'branch)
                (tagged-list? (cadr branch) 'label)))
      (error "Test without a branch -- COMPILE-TO-C" test branch))
  (string-append "if ("
                 (value->c (cdr test) constants)
                 ") goto "
                 (c-identifier (cadadr branch))
                 ";"))

;; Returns the lines of C code for the instructions. Labels that nothing
;; jumps to are left out, since C warns about unused labels.
(define (code->c instructions constants used-labels)
  (define (statements->c statements)
    (map (lambda (statement) (string-append "    " statement)) statements))
  (define (label->c label)
    (if (hash-table-contains? used-labels label)
        (list (string-append (c-identifier label) ":"))
        '()))
  (let loop ((instructions instructions) (lines '()))
    (if (null? instructions)
        (reverse lines)
        (let ((instruction (car instructions)))
          (cond ((symbol? instruction)
                 (loop (cdr instructions)
                       (append (label->c instruction) lines)))
                ((tagged-list? instruction 'test)
                 (loop (cddr instructions)
                       (append (statements->c
                                (list (test-and-branch->c instruction
                                                          (cadr instructions)
                                                          constants)))
                               lines)))
                (else
                 (loop (cdr instructions)
                       (append (reverse
                                (statements->c
                                 (instruction->c instruction constants)))
                               lines))))))))

;;; Programs

(define (operands-of-kind kind instruction)
  (if (symbol? instruction)
      '()
      (map cadr
           (filter (lambda (part) (tagged-list? part kind))
                   (cdr instruction)))))

(define (remove-duplicates items)
  (let ((seen (make-equal-hash-table)))
    (let loop ((items items) (unique '()))
      (cond ((null? items) (reverse unique))
            ((hash-table-contains? seen (car items))
             (loop (cdr items) unique))
            (else
             (hash-table-set! seen (car items) #t)
             (loop (cdr items) (cons (car items) unique)))))))

;; Returns a table that maps each item to its position in the list.
(define (index-table items)
  (let ((table (make-equal-hash-table)))
    (for-each (lambda (item index) (hash-table-set! table item index))
              items
              (iota (length items)))
    table))

(define (collect kind instructions)
  (remove-duplicates
   (append-map (lambda (instruction) (operands-of-kind kind instruction))
               instructions)))

(define (program-constants instructions)
  (filter table-constant? (collect 'const instructions)))

(define (stored-labels instructions)
  (collect 'label (filter (lambda (instruction)
                            (tagged-list? instruction 'assign))
                          instructions)))

(define (computed-goto? instruction)
  (and (tagged-list? instruction 'goto)
       (tagged-list? (cadr instruction) 'reg)))

(define (write-c-program instructions port)
  (let ((constants (program-constants instructions))
        (stored (stored-labels instructions))
        (dispatch? (any computed-goto? instructions)))
    (define (line . strings)
      (for-each (lambda (string) (write-string string port)) strings)
      (newline port))
    (line "/* Compiled from Scheme by compile-to-c.scm. */")
    (line)
    (line "#include \"runtime.h\"")
    (if (pair? stored)
        (begin
          (line)
          (line "enum {")
          (for-each (lambda (label) (line "    " (label-number label) ","))
                    stored)
          (line "};")))
    (if (pair? constants)
        (begin
          (line)
          (line "static const char *const constant_texts[] = {")
          (for-each (lambda (constant)
                      (line "    " (c-string-literal (datum->text constant))
                            ","))
                    constants)
          (line "};")
          (line)
          (line "static Value constants["
                (number->string (length constants)) "];")))
    (line)
    (line "void run_program(void)")
    (line "{")
    (if dispatch? (line "    int target;"))
    (if (pair? constants)
        (line "    load_constants(constants, constant_texts, "
              (number->string (length constants)) ");"))
    (if (or dispatch? (pair? constants)) (line))
    (for-each line (code->c instructions
                            (index-table constants)
                            (index-table (collect 'label instructions))))
    (line "    return;")
    (if dispatch?
        (begin
          (line)
          (line "dispatch:")
          (line "    switch (target) {")
          (for-each (lambda (label)
                      (line "    case " (label-number label)
                            ": goto " (c-identifier label) ";"))
                    stored)
          (line "    }")))
    (line "}")))

(define (compile-to-c exps port)
  (write-c-program (statements (compile (make-begin exps) 'val 'next))
                   port))
