(load "lib/check.scm")

;; The procedural pairs get distinct names so that the built-in cons, car,
;; cdr, set-car! and set-cdr! stay intact.
(define (proc-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (proc-car z) (z 'car))
(define (proc-cdr z) (z 'cdr))
(define (proc-set-car! z new-value) ((z 'set-car!) new-value) z)
(define (proc-set-cdr! z new-value) ((z 'set-cdr!) new-value) z)

;; (define x (cons 1 2)) creates E1 below the global environment with x = 1,
;; y = 2 and the procedures set-x!, set-y! and dispatch, whose environment
;; is E1.  The global x is bound to that dispatch.
;;
;; (define z (cons x x)) creates E2 in the same way, with both x and y bound
;; to the dispatch procedure of E1.  The global z is its own dispatch.
;;
;; (set-car! (cdr z) 17): (cdr z) calls z's dispatch in a frame m = cdr below
;; E2 and returns y of E2, which is the global x.  set-car! then calls x's
;; dispatch (frame m = set-car! below E1), which returns set-x!; applying it
;; makes a frame v = 17 below E1, and set! changes x in E1 to 17.
;;
;; (car x) calls x's dispatch in a frame m = car below E1 and returns 17.

(define x (proc-cons 1 2))
(define z (proc-cons x x))
(proc-set-car! (proc-cdr z) 17)
(check (proc-car x) => 17)
(check (proc-car (proc-car z)) => 17)
(check (proc-cdr x) => 2)

(proc-set-cdr! x 3)
(check (proc-cdr (proc-cdr z)) => 3)
(check-error (x 'length))
