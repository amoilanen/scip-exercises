(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/lazy.scm" (current-load-pathname)))

(define lazy-eval-sequence eval-sequence)

(define (cy-eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (cy-eval-sequence (rest-exps exps) env))))

(define for-each-program
  '((define (for-each proc items)
      (if (null? items)
          'done
          (begin (proc (car items))
                 (for-each proc (cdr items)))))
    (for-each (lambda (x) (newline) (display x))
              (list 57 321 88))))

(define (for-each-output)
  (with-output-to-string
    (lambda () (apply interpret for-each-program))))

(define p-definitions
  '((define (p1 x)
      (set! x (cons x '(2)))
      x)
    (define (p2 x)
      (define (p e)
        e
        x)
      (p (set! x (cons x '(2)))))))

(define (p-results)
  (apply interpret (append p-definitions '((list (p1 1) (p2 1))))))

;; a. Ben is right: (proc (car items)) is an application, so evaluating it
;;    with mc-eval already applies proc, and in the body of proc newline and
;;    display are primitives, which force their arguments.  No thunk whose
;;    side effect matters is left unforced.
;;
;; b. Original: (p1 1) is (1 2), but (p2 1) is 1.  In p the argument e is a
;;    thunk for the set!, and evaluating the variable e in the sequence only
;;    looks the thunk up without forcing it, so the assignment never happens.
;;    With Cy's change both give (1 2).
;;
;; c. actual-value is mc-eval followed by force-it, and force-it leaves a
;;    non-thunk alone.  In a the side effects happen within mc-eval, and the
;;    values of the sequence elements are not thunks, so forcing them
;;    changes nothing.
;;
;; d. Cy's approach.  A non-final expression of a sequence is only there
;;    for its effect, so it should be carried out; otherwise whether a side
;;    effect happens depends on whether some operand thunk is ever forced,
;;    which is hard to predict when reading the program.

(define eval-sequence lazy-eval-sequence)
(check (for-each-output) => "\n57\n321\n88")
(check (p-results) => '((1 2) 1))

(define eval-sequence cy-eval-sequence)
(check (for-each-output) => "\n57\n321\n88")
(check (p-results) => '((1 2) (1 2)))
