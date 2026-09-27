(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "4.16.scm" (current-load-pathname)))

;; Sequential definitions: applying the procedure creates one frame binding
;; the parameters, and the defines add u and v to that frame; <e3> runs there.
;;
;; Scanned out: the let is a lambda application, so evaluating the body
;; creates a second frame, whose enclosing environment is the parameter frame,
;; binding u and v (first to *unassigned*); the set!s and <e3> run in it.
;;
;; The extra frame is not observable in a correct program: a variable bound
;; in either of the two frames has the same value there as in the single
;; frame of the sequential version, and every other variable is looked up in
;; the same enclosing environment.
;;
;; To avoid the frame, define every internal name as *unassigned* at the
;; start of the body and turn the original defines into assignments.

(define scan-out-defines-with-let scan-out-defines)

(define (scan-out-defines body)
  (let ((definitions (internal-definitions body)))
    (append (map (lambda (definition)
                   (list 'define
                         (definition-variable definition)
                         ''*unassigned*))
                 definitions)
            (map (lambda (exp)
                   (if (definition? exp)
                       (definition->assignment exp)
                       exp))
                 body))))

(check (scan-out-defines '((define u 1) (display u) (define (v) u) (v)))
       => '((define u '*unassigned*)
            (define v '*unassigned*)
            (set! u 1)
            (display u)
            (set! v (lambda () u))
            (v)))
(check (scan-out-defines '((+ 1 2))) => '((+ 1 2)))

(check (interpret '(define (f x)
                     (define (even? n) (if (= n 0) true (odd? (- n 1))))
                     (define (odd? n) (if (= n 0) false (even? (- n 1))))
                     (list (even? x) (odd? x)))
                  '(f 8))
       => '(#t #f))
(check (interpret '(define (f x)
                     (define x 5)
                     x)
                  '(f 1))
       => 5)
(check-error (apply interpret shadowing-program))

(define (frames-seen-by-internal-procedure)
  (length (procedure-environment
           (interpret '(define (f) (define (g) 'g) g) '(f)))))

(check (fluid-let ((scan-out-defines scan-out-defines-with-let))
         (frames-seen-by-internal-procedure))
       => 3)
(check (frames-seen-by-internal-procedure) => 2)
