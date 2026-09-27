(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/compiler.scm" (current-load-pathname)))

;; The compiler evaluates operands right to left: construct-arglist reverses
;; the operand codes so that each value can be consed onto argl.

(define order-test
  '(begin
     (define order '())
     (define (note x)
       (set! order (cons x order))
       x)
     (define arguments (list (note 1) (note 2) (note 3)))
     (list arguments order)))

(check (compile-and-run order-test) => '((1 2 3) (1 2 3)))

;; To evaluate them left to right, argl is built in reverse with cons and
;; reversed once all operands are evaluated.  This costs one extra operation
;; that traverses argl on every call with more than one operand.  The saves
;; move around (a call in the last operand now needs argl saved rather than
;; env) but their number stays about the same.  Adding each value to the
;; end of argl instead would take time quadratic in the number of operands.

(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl) '((assign argl (const ()))))
      (let ((code-to-get-first-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence
               '(val) '(argl)
               '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-first-arg
            (append-instruction-sequences
             (preserving '(env)
                         code-to-get-first-arg
                         (code-to-get-rest-args (cdr operand-codes)))
             (make-instruction-sequence
              '(argl) '(argl)
              '((assign argl (op reverse) (reg argl)))))))))

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

(define compiled-code-operations
  (cons (list 'reverse reverse) compiled-code-operations))

(check (compile-and-run order-test) => '((1 2 3) (3 2 1)))
(check (compile-and-run '(define (f) (list)) '(f)) => '())
(check (compile-and-run '(- 10 (* 2 3) 1)) => 3)
(check (compile-and-run '(define (factorial n)
                           (if (= n 1)
                               1
                               (* (factorial (- n 1)) n)))
                        '(factorial 6))
       => 720)
