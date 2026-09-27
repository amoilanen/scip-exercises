(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval-compiled.scm" (current-load-pathname)))

;; The evaluator initializes compapp to its compound-apply entry point (see
;; ch5/lib/eceval-compiled.scm).  compound-apply expects the continuation
;; on the stack, as apply-dispatch does, so it is saved before the jump.

(define compile-procedure-call-for-compiled-code compile-procedure-call)

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((procedure-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target procedure-linkage))
        (parallel-instruction-sequences
         (append-instruction-sequences
          compound-branch
          (compile-compound-appl target procedure-linkage))
         (append-instruction-sequences
          primitive-branch
          (end-with-linkage
           linkage
           (make-instruction-sequence
            '(proc argl) (list target)
            `((assign ,target
                      (op apply-primitive-procedure)
                      (reg proc)
                      (reg argl))))))))
       after-call))))

(define (compile-compound-appl target linkage)
  (define (call-with-return-to return-label)
    `((assign continue (label ,return-label))
      (save continue)
      (goto (reg compapp))))
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    (call-with-return-to linkage)))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            (append (call-with-return-to proc-return)
                    `(,proc-return
                      (assign ,target (reg val))
                      (goto (label ,linkage)))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((save continue)
            (goto (reg compapp)))))
        (else
         (error "return linkage, target not val -- COMPILE" target))))

(define compiled-procedures
  '(begin
     (define (apply-to f x) (f x))
     (define (twice f x) (f (f x)))
     (define (call-and-add f x) (+ (f x) 1))
     (define (call-result f x) ((f) x))))

(compile-and-go compiled-eceval compiled-procedures)
(eceval-interpret compiled-eceval '(define (square x) (* x x)))
(eceval-interpret compiled-eceval '(define (make-square) square))

(check (eceval-interpret compiled-eceval '(apply-to square 3)) => 9)
(check (eceval-interpret compiled-eceval '(twice square 3)) => 81)
(check (eceval-interpret compiled-eceval '(call-and-add square 3)) => 10)
(check (eceval-interpret compiled-eceval '(call-result make-square 5)) => 25)
(check (eceval-interpret compiled-eceval '(twice (lambda (x) (+ x 1)) 0))
       => 2)
(check (eceval-interpret compiled-eceval '(twice (lambda (x) (* x 3)) 1))
       => 9)
(check (eceval-interpret compiled-eceval '(apply-to car '(1 2))) => 1)

;; Tail calls between compiled and interpreted procedures don't grow the
;; stack.
(compile-and-go compiled-eceval '(define (loop n) (count-down n)))
(eceval-interpret compiled-eceval
                  '(define (count-down n)
                     (if (= n 0) 'done (loop (- n 1)))))

(define (maximum-depth n)
  (eceval-interpret compiled-eceval `(loop ,n))
  (cdr (assq 'maximum-depth (stack-statistics compiled-eceval))))

(check (eceval-interpret compiled-eceval '(loop 50)) => 'done)
(check (maximum-depth 5) => (maximum-depth 50))

;; Without the compound branch an interpreted procedure is taken for a
;; compiled one, and its parameter list for its entry point.
(fluid-let ((compile-procedure-call compile-procedure-call-for-compiled-code))
  (compile-and-go compiled-eceval '(define (apply-to f x) (f x))))
(eceval-interpret compiled-eceval '(define (square x) (* x x)))
(check-error (eceval-interpret compiled-eceval '(apply-to square 3)))
