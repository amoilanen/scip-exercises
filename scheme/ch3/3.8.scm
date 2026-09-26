(load "lib/check.scm")

;; f returns the argument of the previous call (0 on the first call).
(define (make-f)
  (let ((previous 0))
    (lambda (x)
      (let ((result previous))
        (set! previous x)
        result))))

(check (let* ((f (make-f))
              (left (f 0))
              (right (f 1)))
         (+ left right))
       => 0)

(check (let* ((f (make-f))
              (right (f 1))
              (left (f 0)))
         (+ left right))
       => 1)

;; MIT Scheme evaluates operands from right to left.
(define f (make-f))
(check (+ (f 0) (f 1)) => 1)
